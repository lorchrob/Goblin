#!/usr/bin/env python3
"""
FTP Fuzzing Driver with Runtime Monitor + Predicate Coverage
=============================================================
Black-box driver that bridges the OCaml grammar fuzzer with a live FTP target,
feeds predicate events to the LTL runtime monitor, and uses predicate-combination
coverage as a feedback metric for the fuzzer.

Architecture:
  OCaml fuzzer  <--file IPC-->  this driver  <--stdin/stdout-->  formula_parser
                                     |
                                     v
                                FTP server (TCP)

IPC Protocol (file-based, matching grammarFuzzing.ml):
  OCaml -> Driver:  sync/message.txt           (comma-separated trace)
  Driver -> OCaml:  sync/response.txt          (CRASH | TIMEOUT | EXPECTED_OUTPUT | UNEXPECTED_OUTPUT)
  Driver -> OCaml:  sync/oracle-response.txt   (integer state ID)
  Driver -> OCaml:  sync/score.txt             (line 1: "interesting"|"", line 2: float score)
"""

import argparse
import json
import logging
import os
import re
import signal
import socket
import ssl
import subprocess
import sys
import time
import traceback
from dataclasses import dataclass
from enum import IntEnum
from pathlib import Path
from typing import Optional

from ftp_monitor import FTPMonitor, SessionResult


# ---------------------------------------------------------------------------
# Configuration
# ---------------------------------------------------------------------------

@dataclass
class FTPTargetConfig:
    host: str = "127.0.0.1"
    port: int = 21
    timeout: float = 5.0
    use_tls: bool = False
    username: str = "anonymous"
    password: str = "fuzz@fuzz.com"
    max_trace_len: int = 20
    reconnect_delay: float = 0.5
    poll_interval: float = 0.05
    crash_detect_retries: int = 3
    crash_detect_delay: float = 1.0


@dataclass
class ServerConfig:
    """Configuration for managed FTP server process."""
    binary: str = ""           # Path to fftp binary (empty = unmanaged)
    server_config: str = ""    # Path to fftp.conf
    profraw_dir: str = ""      # Where to write .profraw files
    restart_every: int = 1     # Restart process every N traces (1 = every trace)
    startup_wait: float = 1.0  # Seconds to wait after starting server
    stop_timeout: float = 5.0  # Seconds to wait for SIGTERM before SIGKILL


class FTPState(IntEnum):
    DISCONNECTED = 0
    CONNECTED = 1
    AUTH_USER = 2
    AUTHENTICATED = 3


class DriverOutput:
    CRASH = "CRASH"
    TIMEOUT = "TIMEOUT"
    EXPECTED = "EXPECTED_OUTPUT"
    UNEXPECTED = "UNEXPECTED_OUTPUT"


# ---------------------------------------------------------------------------
# FTP Server process manager
# ---------------------------------------------------------------------------

class FTPProcessManager:
    """
    Manages the LightFTP server process lifecycle for fuzzing.

    - Starts fftp with LLVM_PROFILE_FILE set so each run writes a .profraw
    - Stops with SIGTERM (flushes coverage counters) before restarting
    - Falls back to SIGKILL if the process won't exit cleanly
    - Tracks run count for unique profraw filenames
    """

    def __init__(self, config: ServerConfig):
        self.config = config
        self.proc: Optional[subprocess.Popen] = None
        self.run_count = 0
        self.logger = logging.getLogger("server_mgr")

        if self.config.profraw_dir:
            os.makedirs(self.config.profraw_dir, exist_ok=True)

    @property
    def managed(self) -> bool:
        """True if we're managing the server process (binary was provided)."""
        return bool(self.config.binary)

    @property
    def alive(self) -> bool:
        return self.proc is not None and self.proc.poll() is None

    def start(self) -> bool:
        """Start the FTP server process. Returns True on success."""
        if not self.managed:
            return True  # External process, nothing to do

        if self.alive:
            self.logger.debug("Server already running (PID %d)", self.proc.pid)
            return True

        self.run_count += 1

        env = os.environ.copy()
        if self.config.profraw_dir:
            profile_pattern = os.path.join(
                self.config.profraw_dir,
                f"fftp-{self.run_count:06d}-%p-%m.profraw",
            )
            env["LLVM_PROFILE_FILE"] = profile_pattern

        try:
            self.proc = subprocess.Popen(
                [self.config.binary, self.config.server_config],
                env=env,
                stdout=subprocess.DEVNULL,
                stderr=subprocess.PIPE,
            )
        except OSError as e:
            self.logger.error("Failed to start server: %s", e)
            return False

        time.sleep(self.config.startup_wait)

        if not self.alive:
            stderr = ""
            if self.proc.stderr:
                stderr = self.proc.stderr.read().decode(errors="replace")[:300]
            self.logger.error(
                "Server exited immediately (code %d): %s",
                self.proc.returncode, stderr,
            )
            self.proc = None
            return False

        self.logger.info(
            "Server started — PID %d (run #%d)", self.proc.pid, self.run_count,
        )
        return True

    def stop(self) -> bool:
        """
        Stop the server with SIGTERM (flushes .profraw), falling back to SIGKILL.
        Returns True if the process exited cleanly.
        """
        if not self.managed or self.proc is None:
            return True

        if not self.alive:
            self.logger.debug("Server already exited (code %d)", self.proc.returncode)
            self.proc = None
            return True

        pid = self.proc.pid
        self.logger.debug("Stopping server (PID %d) with SIGTERM...", pid)
        self.proc.terminate()

        try:
            self.proc.wait(timeout=self.config.stop_timeout)
            self.logger.debug("Server exited cleanly (code %d)", self.proc.returncode)
            self.proc = None
            return True
        except subprocess.TimeoutExpired:
            self.logger.warning("Server didn't exit in %.1fs — SIGKILL",
                                self.config.stop_timeout)
            self.proc.kill()
            try:
                self.proc.wait(timeout=3.0)
            except subprocess.TimeoutExpired:
                pass
            self.proc = None
            return False

    def restart(self) -> bool:
        """Stop then start. Returns True if the new instance started."""
        self.stop()
        return self.start()


# ---------------------------------------------------------------------------
# FTP command parsing
# ---------------------------------------------------------------------------

def extract_ftp_verb(data: bytes) -> str:
    try:
        text = data.decode("utf-8", errors="replace").strip()
    except Exception:
        return ""
    match = re.match(r"([A-Za-z]{3,4})\b", text)
    if match:
        return match.group(1).upper()
    return ""


# ---------------------------------------------------------------------------
# FTP Session
# ---------------------------------------------------------------------------

class FTPSession:
    def __init__(self, config: FTPTargetConfig):
        self.config = config
        self.sock: Optional[socket.socket] = None
        self.state = FTPState.DISCONNECTED
        self.last_response: str = ""
        self.last_code: int = 0
        self.banner_bytes: bytes = b""
        self.logger = logging.getLogger("ftp_session")

    def connect(self) -> bool:
        self.close()
        self.banner_bytes = b""
        try:
            raw = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
            raw.settimeout(self.config.timeout)
            raw.connect((self.config.host, self.config.port))

            if self.config.use_tls:
                ctx = ssl.create_default_context()
                ctx.check_hostname = False
                ctx.verify_mode = ssl.CERT_NONE
                self.sock = ctx.wrap_socket(raw, server_hostname=self.config.host)
            else:
                self.sock = raw

            banner = self._recv()
            if banner is None:
                self.logger.warning("No banner received")
                self.close()
                return False

            self.banner_bytes = banner.encode("utf-8", errors="replace")
            self.state = FTPState.CONNECTED
            self.logger.info("Connected. Banner: %s", banner.strip())
            return True

        except (socket.error, ssl.SSLError, OSError) as exc:
            self.logger.warning("Connection failed: %s", exc)
            self.close()
            return False

    def close(self):
        if self.sock:
            try:
                self.sock.shutdown(socket.SHUT_RDWR)
            except OSError:
                pass
            try:
                self.sock.close()
            except OSError:
                pass
        self.sock = None
        self.state = FTPState.DISCONNECTED

    def _recv(self, bufsize: int = 4096) -> Optional[str]:
        if self.sock is None:
            return None
        try:
            chunks = []
            while True:
                data = self.sock.recv(bufsize)
                if not data:
                    return None
                chunks.append(data)
                combined = b"".join(chunks)
                if combined.endswith(b"\r\n"):
                    break
            resp = combined.decode("utf-8", errors="replace")
            self.last_response = resp
            try:
                self.last_code = int(resp[:3])
            except (ValueError, IndexError):
                self.last_code = 0
            return resp
        except socket.timeout:
            return None
        except (socket.error, OSError):
            return None

    def send_raw(self, data: bytes) -> Optional[str]:
        if self.sock is None:
            return None
        try:
            self.sock.sendall(data)
            return self._recv()
        except (socket.error, ssl.SSLError, OSError, BrokenPipeError):
            return None

    def send_line(self, line: str) -> Optional[str]:
        return self.send_raw((line + "\r\n").encode("utf-8", errors="replace"))


# ---------------------------------------------------------------------------
# Trace parser
# ---------------------------------------------------------------------------

def parse_trace(raw_msg: str) -> list:
    tokens = [t.strip() for t in raw_msg.split(",") if t.strip()]
    trace = []
    for tok in tokens:
        try:
            val = int(tok)
            trace.append(("valid", val))
            continue
        except ValueError:
            pass
        try:
            raw_bytes = bytes.fromhex(tok)
            trace.append(("raw", raw_bytes))
        except ValueError:
            logging.getLogger("trace_parser").warning(
                "Skipping unparseable token: %r", tok
            )
    return trace


# ---------------------------------------------------------------------------
# Valid-packet handlers
# ---------------------------------------------------------------------------

FTP_VALID_COMMANDS = {
    0: "USER {username}",
    1: "PASS {password}",
    2: "CWD /",
    3: "PWD",
    4: "TYPE I",
    5: "PASV",
    6: "LIST",
    7: "SYST",
    8: "FEAT",
    9: "NOOP",
}


def valid_packet_to_bytes(pkt_id: int, config: FTPTargetConfig) -> bytes:
    template = FTP_VALID_COMMANDS.get(pkt_id)
    if template is None:
        return b""
    cmd = template.format(username=config.username, password=config.password)
    return (cmd + "\r\n").encode("utf-8")


# ---------------------------------------------------------------------------
# Response classification
# ---------------------------------------------------------------------------

def classify_response(
    session: FTPSession,
    response: Optional[str],
    config: FTPTargetConfig,
) -> str:
    if response is None:
        if session.sock is None:
            return DriverOutput.CRASH
        for _ in range(config.crash_detect_retries):
            time.sleep(config.crash_detect_delay)
            try:
                probe = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
                probe.settimeout(2.0)
                probe.connect((config.host, config.port))
                probe.close()
                return DriverOutput.TIMEOUT
            except (socket.error, OSError):
                continue
        return DriverOutput.CRASH

    stripped = response.strip()
    if len(stripped) >= 3 and stripped[:3].isdigit():
        code = int(stripped[:3])
        if 100 <= code <= 599:
            return DriverOutput.EXPECTED
    return DriverOutput.UNEXPECTED


def compute_oracle_state(session: FTPSession) -> int:
    return int(session.state)


# ---------------------------------------------------------------------------
# Sync-file helpers
# ---------------------------------------------------------------------------

class SyncFiles:
    def __init__(self, sync_dir: str = "sync"):
        self.sync_dir = Path(sync_dir)
        self.message_file = self.sync_dir / "message.txt"
        self.response_file = self.sync_dir / "response.txt"
        self.oracle_file = self.sync_dir / "oracle-response.txt"
        self.score_file = self.sync_dir / "score.txt"
        self.logger = logging.getLogger("sync")

    def ensure_files(self):
        self.sync_dir.mkdir(parents=True, exist_ok=True)
        for f in [self.message_file, self.response_file,
                  self.oracle_file, self.score_file]:
            f.touch(exist_ok=True)

    def read_message(self) -> Optional[str]:
        try:
            content = self.message_file.read_text().strip()
            return content if content else None
        except (OSError, IOError):
            return None

    def clear_message(self):
        try:
            self.message_file.write_text("")
        except (OSError, IOError):
            pass

    def write_response(self, output: str):
        try:
            self.response_file.write_text(output)
        except (OSError, IOError) as e:
            self.logger.error("Failed to write response: %s", e)

    def write_oracle(self, state: int):
        try:
            self.oracle_file.write_text(str(state))
        except (OSError, IOError) as e:
            self.logger.error("Failed to write oracle: %s", e)

    def write_score(self, is_interesting: bool, score: float):
        """Write coverage score for the OCaml fuzzer.
        Format matches wait_for_score in grammarFuzzing.ml:
          line 1: "interesting" or ""
          line 2: float score
        """
        try:
            label = "interesting" if is_interesting else ""
            self.score_file.write_text(f"{label}\n{score:.6f}\n")
        except (OSError, IOError) as e:
            self.logger.error("Failed to write score: %s", e)


# ---------------------------------------------------------------------------
# Main driver loop
# ---------------------------------------------------------------------------

class FTPFuzzDriver:
    """
    Main driver: polls sync/message.txt, replays traces against the FTP
    target, classifies responses, feeds predicates to the runtime monitor,
    tracks predicate-combination coverage, and writes results + scores back
    for the fuzzer.

    When server_config is provided (--server-binary), the driver manages the
    FTP process lifecycle: starting, stopping (with SIGTERM to flush profraw),
    and restarting every N traces for clean state isolation.
    """

    def __init__(self, config: FTPTargetConfig, sync_dir: str = "sync",
                 monitor: Optional[FTPMonitor] = None,
                 server_config: Optional[ServerConfig] = None):
        self.config = config
        self.sync = SyncFiles(sync_dir)
        self.session = FTPSession(config)
        self.monitor = monitor
        self.server = FTPProcessManager(server_config or ServerConfig())
        self.logger = logging.getLogger("driver")
        self.running = True
        self._traces_since_restart = 0
        self.stats = {
            "traces_executed": 0,
            "crashes": 0,
            "timeouts": 0,
            "expected": 0,
            "unexpected": 0,
            "violations": 0,
            "interesting_traces": 0,
            "total_coverage_score": 0.0,
            "server_restarts": 0,
        }

    def _setup_signals(self):
        def handler(signum, frame):
            self.logger.info("Signal %d received, shutting down...", signum)
            self.running = False
        signal.signal(signal.SIGINT, handler)
        signal.signal(signal.SIGTERM, handler)

    def _ensure_connected(self) -> bool:
        if self.session.state != FTPState.DISCONNECTED and self.session.sock:
            return True
        self.logger.info("Connecting to %s:%d ...", self.config.host, self.config.port)
        return self.session.connect()

    def _reset_connection(self):
        """Close the TCP socket only. Does NOT touch the server process."""
        self.session.close()

    def _restart_server_if_due(self):
        """
        After a trace completes and results are written, restart the FTP
        server process if it's time.  SIGTERM flushes the .profraw file.

        Called from the main loop AFTER write_score, so the OCaml fuzzer
        gets its results promptly and the restart cost doesn't block IPC.
        """
        if not self.server.managed:
            return

        self._traces_since_restart += 1
        restart_every = self.server.config.restart_every

        need_restart = False
        if not self.server.alive:
            self.logger.warning("Server process died — restarting")
            need_restart = True
        elif restart_every > 0 and self._traces_since_restart >= restart_every:
            self.logger.debug(
                "Restarting server (every %d traces, %d since last)",
                restart_every, self._traces_since_restart,
            )
            need_restart = True

        if need_restart:
            # SIGTERM → profraw flush → start fresh
            if not self.server.restart():
                self.logger.error("Server restart failed — retrying once")
                time.sleep(1.0)
                if not self.server.restart():
                    self.logger.error("Server restart failed twice")
            self._traces_since_restart = 0
            self.stats["server_restarts"] += 1

    def _feed_command(self, cmd_bytes: bytes):
        if self.monitor and self.monitor.alive:
            self.monitor.feed_command(cmd_bytes)

    def _feed_response(self, resp_bytes: bytes):
        if self.monitor and self.monitor.alive:
            self.monitor.feed_response(resp_bytes)
            if self.monitor.check_violation():
                self.stats["violations"] += 1
                self.logger.warning("VIOLATION detected (total: %d)",
                                    self.stats["violations"])
                self.monitor.clear_violation()

    def _feed_timeout(self):
        if self.monitor and self.monitor.alive:
            self.monitor.feed_timeout()

    def execute_trace(self, trace: list) -> tuple:
        """
        Execute a full trace against the FTP target.
        Returns (driver_output, oracle_state, session_result).
        session_result is None if monitor is not attached.

        Flow:
          1. Close any existing TCP connection
          2. Connect fresh to the (already-running) server
          3. Replay the trace
          4. Collect monitor results
          5. Close TCP

        The caller (run loop) handles server process restart AFTER this returns.
        """
        # Close previous TCP session
        self._reset_connection()
        time.sleep(self.config.reconnect_delay)

        if self.monitor:
            self.monitor.adapter.reset_session()

        if not self._ensure_connected():
            # No monitor events to record for a failed connection
            return DriverOutput.CRASH, int(FTPState.DISCONNECTED), None

        # Feed the banner as a response event
        if self.session.banner_bytes:
            self._feed_response(self.session.banner_bytes)

        last_output = DriverOutput.EXPECTED
        response = None

        for i, (kind, payload) in enumerate(trace):
            cmd_bytes = b""

            if kind == "valid":
                self.logger.debug("  [%d] ValidPacket(%d)", i, payload)
                cmd_bytes = valid_packet_to_bytes(payload, self.config)

                if payload == 0:
                    self._feed_command(cmd_bytes)
                    resp = self.session.send_line(f"USER {self.config.username}")
                    if resp and resp.startswith("331"):
                        self.session.state = FTPState.AUTH_USER
                    elif resp and resp.startswith("230"):
                        self.session.state = FTPState.AUTHENTICATED
                    response = resp
                elif payload == 1:
                    self._feed_command(cmd_bytes)
                    resp = self.session.send_line(f"PASS {self.config.password}")
                    if resp and resp.startswith("230"):
                        self.session.state = FTPState.AUTHENTICATED
                    response = resp
                else:
                    template = FTP_VALID_COMMANDS.get(payload)
                    if template:
                        cmd = template.format(
                            username=self.config.username,
                            password=self.config.password,
                        )
                        self._feed_command(cmd_bytes)
                        response = self.session.send_line(cmd)
                    else:
                        response = None

            elif kind == "raw":
                self.logger.debug("  [%d] RawPacket(%d bytes)", i, len(payload))
                cmd_bytes = payload
                self._feed_command(cmd_bytes)
                response = self.session.send_raw(payload)

            # Feed the response (or timeout) to the monitor
            was_timeout = (response is None and self.session.sock is not None)
            if was_timeout:
                self._feed_timeout()
            elif response is not None:
                resp_bytes = response.encode("utf-8", errors="replace")
                self._feed_response(resp_bytes)
            else:
                self._feed_timeout()

            last_output = classify_response(self.session, response, self.config)

            if last_output == DriverOutput.CRASH:
                self.logger.warning("Crash detected at trace step %d", i)
                break

        # End the monitor session and collect results
        oracle_state = compute_oracle_state(self.session)
        session_result = None

        if self.monitor and self.monitor.alive:
            session_result = self.monitor.end_session()
            if session_result.violated:
                self.stats["violations"] += 1
                self.logger.warning(
                    "VIOLATION on end_session (total: %d)",
                    self.stats["violations"],
                )

        # Close TCP — server process stays alive until the run loop restarts it
        self._reset_connection()

        return last_output, oracle_state, session_result

    def run(self):
        """Main polling loop."""
        self._setup_signals()
        self.sync.ensure_files()

        # Start the managed server process (no-op if unmanaged)
        if self.server.managed:
            if not self.server.start():
                self.logger.error("Failed to start FTP server — aborting")
                return
            self.logger.info(
                "Managed server: %s (restart every %d traces)",
                self.server.config.binary,
                self.server.config.restart_every,
            )

        if self.monitor:
            self.monitor.start()
            if not self.monitor.alive:
                self.logger.error("Monitor failed to start — continuing without it")
                self.monitor = None

        self.logger.info(
            "FTP Fuzz Driver started — target %s:%d — polling %s — monitor %s",
            self.config.host,
            self.config.port,
            self.sync.sync_dir,
            "attached" if self.monitor else "disabled",
        )

        try:
            while self.running:
                msg = self.sync.read_message()
                if msg is None:
                    time.sleep(self.config.poll_interval)
                    continue

                self.sync.clear_message()
                self.stats["traces_executed"] += 1
                trace_num = self.stats["traces_executed"]

                self.logger.info("=== Trace #%d ===", trace_num)
                self.logger.debug("Raw message: %s", msg[:200])

                trace = parse_trace(msg)
                if not trace:
                    self.logger.warning("Empty trace, responding EXPECTED_OUTPUT")
                    self.sync.write_oracle(0)
                    self.sync.write_response(DriverOutput.EXPECTED)
                    self.sync.write_score(False, 0.0)
                    self._restart_server_if_due()
                    continue

                try:
                    output, oracle_state, session_result = self.execute_trace(trace)
                except Exception as exc:
                    self.logger.error(
                        "Unhandled exception: %s\n%s", exc, traceback.format_exc()
                    )
                    output = DriverOutput.CRASH
                    oracle_state = 0
                    session_result = None

                stat_key = {
                    DriverOutput.CRASH: "crashes",
                    DriverOutput.TIMEOUT: "timeouts",
                    DriverOutput.EXPECTED: "expected",
                    DriverOutput.UNEXPECTED: "unexpected",
                }.get(output, "unexpected")
                self.stats[stat_key] += 1

                # Extract coverage score
                coverage_score = 0.0
                is_interesting = False
                if session_result is not None:
                    coverage_score = session_result.coverage_score
                    is_interesting = session_result.is_interesting
                    if is_interesting:
                        self.stats["interesting_traces"] += 1
                    self.stats["total_coverage_score"] += coverage_score

                # Write all results for the OCaml fuzzer
                self.sync.write_oracle(oracle_state)
                self.sync.write_response(output)
                self.sync.write_score(is_interesting, coverage_score)

                # Restart server AFTER results are written so OCaml gets
                # its response promptly. SIGTERM flushes .profraw.
                self._restart_server_if_due()

                # Log with coverage info
                cov_str = ""
                if session_result is not None:
                    cov_str = (
                        f" | Coverage: score={coverage_score:.1f}"
                        f" new_fp={session_result.new_fingerprints}"
                        f" new_trans={session_result.new_transitions}"
                        f" new_rvec={session_result.new_rule_vectors}"
                        f" prox={session_result.proximity_steps}"
                    )
                    if is_interesting:
                        cov_str += " [INTERESTING]"

                self.logger.info(
                    "Result: %s | Oracle: %d%s",
                    output, oracle_state, cov_str,
                )

                # Periodic coverage summary
                if trace_num % 100 == 0 and self.monitor:
                    cs = self.monitor.coverage.stats()
                    self.logger.info(
                        "Coverage @ trace %d: %d fps, %d trans, %d rvecs, "
                        "proximity=%s",
                        trace_num,
                        cs.total_fingerprints,
                        cs.total_transitions,
                        cs.total_rule_vectors,
                        cs.max_proximity,
                    )

        finally:
            self.session.close()
            if self.server.managed:
                self.server.stop()
                self.logger.info(
                    "Server stopped (%d restarts total)",
                    self.stats["server_restarts"],
                )
            if self.monitor:
                cs = self.monitor.coverage.stats()
                self.monitor.stop()
                self.logger.info(
                    "Final coverage: %d fingerprints, %d transitions, "
                    "%d rule vectors, proximity=%s",
                    cs.total_fingerprints, cs.total_transitions,
                    cs.total_rule_vectors, cs.max_proximity,
                )
            self.logger.info(
                "Driver stopped. Final stats: %s", json.dumps(self.stats, indent=2)
            )


# ---------------------------------------------------------------------------
# CLI
# ---------------------------------------------------------------------------

def main():
    parser = argparse.ArgumentParser(
        description="FTP Fuzzing Driver with LTL runtime monitor + predicate coverage"
    )
    parser.add_argument("--host", default="127.0.0.1", help="FTP target host")
    parser.add_argument("--port", type=int, default=21, help="FTP target port")
    parser.add_argument("--timeout", type=float, default=5.0,
                        help="Socket timeout (seconds)")
    parser.add_argument("--tls", action="store_true", help="Use implicit TLS (FTPS)")
    parser.add_argument("--user", default="anonymous", help="FTP username")
    parser.add_argument("--password", default="fuzz@fuzz.com", help="FTP password")
    parser.add_argument("--sync-dir", default="sync", help="Path to sync directory")
    parser.add_argument("--reconnect-delay", type=float, default=0.5)
    parser.add_argument("--poll-interval", type=float, default=0.05)
    parser.add_argument("--log-level", default="INFO",
                        choices=["DEBUG", "INFO", "WARNING", "ERROR"])

    parser.add_argument("--monitor", default=None,
                        help="Path to formula_parser executable (omit to disable)")
    parser.add_argument("--spec", default=None,
                        help="Path to LTL spec file (e.g. ftp.txt)")

    # Server process management
    parser.add_argument("--server-binary", default="",
                        help="Path to fftp binary — driver manages its lifecycle "
                             "(omit to connect to externally-managed server)")
    parser.add_argument("--server-config", default="",
                        help="Path to fftp.conf (required with --server-binary)")
    parser.add_argument("--profraw-dir", default="",
                        help="Directory for .profraw files (coverage data)")
    parser.add_argument("--restart-every", type=int, default=1,
                        help="Restart FTP server every N traces (default: 1 = every trace)")
    parser.add_argument("--startup-wait", type=float, default=1.0,
                        help="Seconds to wait after starting FTP server (default: 1.0)")

    args = parser.parse_args()

    logging.basicConfig(
        level=getattr(logging, args.log_level),
        format="%(asctime)s [%(name)s] %(levelname)s: %(message)s",
        datefmt="%H:%M:%S",
    )

    config = FTPTargetConfig(
        host=args.host,
        port=args.port,
        timeout=args.timeout,
        use_tls=args.tls,
        username=args.user,
        password=args.password,
        reconnect_delay=args.reconnect_delay,
        poll_interval=args.poll_interval,
    )

    # Validate server management args
    srv_config = None
    if args.server_binary:
        if not args.server_config:
            logging.getLogger("main").error(
                "--server-config is required when using --server-binary"
            )
            sys.exit(1)
        if not Path(args.server_binary).exists():
            logging.getLogger("main").error(
                "Server binary not found: %s", args.server_binary
            )
            sys.exit(1)
        if not Path(args.server_config).exists():
            logging.getLogger("main").error(
                "Server config not found: %s", args.server_config
            )
            sys.exit(1)
        srv_config = ServerConfig(
            binary=str(Path(args.server_binary).resolve()),
            server_config=str(Path(args.server_config).resolve()),
            profraw_dir=str(Path(args.profraw_dir).resolve()) if args.profraw_dir else "",
            restart_every=args.restart_every,
            startup_wait=args.startup_wait,
        )

    monitor = None
    if args.monitor and args.spec:
        monitor = FTPMonitor(args.monitor, args.spec)
    elif args.monitor or args.spec:
        logging.getLogger("main").warning(
            "Both --monitor and --spec are required to enable the runtime monitor"
        )

    driver = FTPFuzzDriver(config, sync_dir=args.sync_dir, monitor=monitor,
                           server_config=srv_config)
    driver.run()


if __name__ == "__main__":
    main()