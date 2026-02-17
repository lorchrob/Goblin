#!/usr/bin/env python3
"""
RTSP Fuzzing Driver with Runtime Monitor + Predicate Coverage
==============================================================
Black-box driver that bridges the OCaml grammar fuzzer with a live RTSP target
(e.g. live555), feeds predicate events to the LTL runtime monitor, and uses
predicate-combination coverage as a feedback metric.

Architecture:
  OCaml fuzzer  <--file IPC-->  this driver  <--stdin/stdout-->  formula_parser
                                     |
                                     v
                                RTSP server (TCP)

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
import select
import signal
import socket
import struct
import subprocess
import sys
import time
import traceback
from dataclasses import dataclass
from enum import IntEnum
from pathlib import Path
from typing import Optional

from rtsp_monitor import RTSPMonitor, SessionResult


# ---------------------------------------------------------------------------
# Configuration
# ---------------------------------------------------------------------------

@dataclass
class RTSPTargetConfig:
    host: str = "127.0.0.1"
    port: int = 8554
    timeout: float = 2.0
    stream_path: str = "/test.264"   # RTSP stream path on server
    max_trace_len: int = 20
    reconnect_delay: float = 0.5
    poll_interval: float = 0.05
    crash_detect_retries: int = 1
    crash_detect_delay: float = 0.3


@dataclass
class ServerConfig:
    """Configuration for managed RTSP server process."""
    binary: str = ""
    server_dir: str = ""       # Working directory for live555 (media files)
    profraw_dir: str = ""
    restart_every: int = 1
    startup_wait: float = 1.0
    stop_timeout: float = 5.0
    use_forkserver: bool = False
    child_wait: float = 0.05
    port: int = 8554


class RTSPState(IntEnum):
    DISCONNECTED = 0
    CONNECTED = 1
    DESCRIBED = 2
    READY = 3       # after SETUP
    PLAYING = 4
    RECORDING = 5


class DriverOutput:
    CRASH = "CRASH"
    TIMEOUT = "TIMEOUT"
    EXPECTED = "EXPECTED_OUTPUT"
    UNEXPECTED = "UNEXPECTED_OUTPUT"


# ---------------------------------------------------------------------------
# RTSP Server process manager
# ---------------------------------------------------------------------------

class RTSPProcessManager:
    """Manages the live555MediaServer process lifecycle."""

    def __init__(self, config: ServerConfig):
        self.config = config
        self.proc: Optional[subprocess.Popen] = None
        self.run_count = 0
        self.logger = logging.getLogger("server_mgr")

        if self.config.profraw_dir:
            os.makedirs(self.config.profraw_dir, exist_ok=True)

    @property
    def managed(self) -> bool:
        return bool(self.config.binary)

    @property
    def alive(self) -> bool:
        return self.proc is not None and self.proc.poll() is None

    def start(self) -> bool:
        if not self.managed:
            return True
        if self.alive:
            return True

        self.run_count += 1

        env = os.environ.copy()
        if self.config.profraw_dir:
            profile_pattern = os.path.join(
                self.config.profraw_dir,
                f"rtsp-{self.run_count:06d}-%p-%m.profraw",
            )
            env["LLVM_PROFILE_FILE"] = profile_pattern
            if self.run_count == 1:
                self.logger.info("LLVM_PROFILE_FILE=%s", profile_pattern)
        elif self.run_count == 1:
            self.logger.warning("No profraw_dir — coverage data will NOT be collected")

        cwd = self.config.server_dir if self.config.server_dir else None

        try:
            self.proc = subprocess.Popen(
                [self.config.binary],
                env=env,
                cwd=cwd,
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

        self.logger.info("Server started — PID %d (run #%d)", self.proc.pid, self.run_count)
        return True

    def stop(self) -> bool:
        if not self.managed or self.proc is None:
            return True
        if not self.alive:
            self.proc = None
            return True

        pid = self.proc.pid
        self.proc.terminate()
        try:
            self.proc.wait(timeout=self.config.stop_timeout)
            self.proc = None
            return True
        except subprocess.TimeoutExpired:
            self.logger.warning("Server didn't exit — SIGKILL")
            self.proc.kill()
            try:
                self.proc.wait(timeout=3.0)
            except subprocess.TimeoutExpired:
                pass
            self.proc = None
            return False

    def restart(self) -> bool:
        self.stop()
        return self.start()


# ---------------------------------------------------------------------------
# Forkserver manager (same protocol as FTP)
# ---------------------------------------------------------------------------

class ForkserverManager:
    """Manages RTSP server via AFL-style forkserver."""

    FORKSERVER_FD = 198

    def __init__(self, config: ServerConfig):
        self.config = config
        self.proc: Optional[subprocess.Popen] = None
        self.child_pid: Optional[int] = None
        self.run_count = 0
        self.logger = logging.getLogger("forkserver")
        self._ctl_w: Optional[int] = None
        self._st_r: Optional[int] = None

        if self.config.profraw_dir:
            os.makedirs(self.config.profraw_dir, exist_ok=True)

    @property
    def managed(self) -> bool:
        return bool(self.config.binary)

    @property
    def alive(self) -> bool:
        return self.proc is not None and self.proc.poll() is None

    def start(self) -> bool:
        if not self.managed:
            return True
        if self.alive:
            return True

        ctl_r, ctl_w = os.pipe()
        st_r, st_w = os.pipe()

        env = os.environ.copy()
        env["FORKSERVER_FD"] = str(self.FORKSERVER_FD)

        if self.config.profraw_dir:
            env["LLVM_PROFILE_FILE"] = os.path.join(
                self.config.profraw_dir, "rtsp-%p-%m.profraw",
            )

        target_ctl = self.FORKSERVER_FD
        target_st = self.FORKSERVER_FD + 1
        cwd = self.config.server_dir if self.config.server_dir else None

        def _setup_child_fds():
            if ctl_r != target_ctl:
                os.dup2(ctl_r, target_ctl)
                os.close(ctl_r)
            if st_w != target_st:
                os.dup2(st_w, target_st)
                os.close(st_w)

        try:
            self.proc = subprocess.Popen(
                [self.config.binary],
                env=env,
                cwd=cwd,
                stdout=subprocess.DEVNULL,
                stderr=subprocess.PIPE,
                preexec_fn=_setup_child_fds,
                pass_fds=(ctl_r, st_w),
            )
        except OSError as e:
            self.logger.error("Failed to start forkserver: %s", e)
            for fd in (ctl_r, ctl_w, st_r, st_w):
                os.close(fd)
            return False

        os.close(ctl_r)
        os.close(st_w)
        self._ctl_w = ctl_w
        self._st_r = st_r

        handshake = self._read_st(timeout=self.config.startup_wait + 5.0)
        if handshake is None:
            self.logger.error("Forkserver handshake timed out")
            self._cleanup()
            return False

        self.logger.info("Forkserver ready — PID %d", self.proc.pid)
        return True

    def fork_child(self) -> bool:
        if not self.alive:
            return False
        self.run_count += 1
        try:
            os.write(self._ctl_w, b"\x00\x00\x00\x00")
        except OSError:
            return False

        pid_bytes = self._read_st(timeout=5.0)
        if pid_bytes is None:
            return False

        self.child_pid = struct.unpack("I", pid_bytes)[0]
        if self.child_pid == 0:
            return False

        self._wait_for_port()
        self.logger.debug("Child forked — PID %d (run #%d)", self.child_pid, self.run_count)
        return True

    def kill_child(self) -> bool:
        if self.child_pid is None:
            return True
        try:
            os.kill(self.child_pid, signal.SIGTERM)
        except ProcessLookupError:
            pass
        except OSError:
            pass
        status_bytes = self._read_st(timeout=self.config.stop_timeout)
        self.child_pid = None
        return True

    def restart(self) -> bool:
        self.kill_child()
        return self.fork_child()

    def stop(self) -> bool:
        self.kill_child()
        self._cleanup()
        return True

    def _read_st(self, timeout: float = 5.0) -> Optional[bytes]:
        if self._st_r is None:
            return None
        ready, _, _ = select.select([self._st_r], [], [], timeout)
        if not ready:
            return None
        try:
            data = os.read(self._st_r, 4)
            return data if len(data) == 4 else None
        except OSError:
            return None

    def _wait_for_port(self):
        deadline = time.time() + 2.0
        while time.time() < deadline:
            try:
                probe = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
                probe.settimeout(0.1)
                probe.connect(("127.0.0.1", self.config.port))
                probe.close()
                return
            except (socket.error, OSError):
                time.sleep(self.config.child_wait)

    def _cleanup(self):
        for fd_attr in ('_ctl_w', '_st_r'):
            fd = getattr(self, fd_attr, None)
            if fd is not None:
                try:
                    os.close(fd)
                except OSError:
                    pass
                setattr(self, fd_attr, None)
        if self.proc is not None:
            try:
                self.proc.terminate()
                self.proc.wait(timeout=3.0)
            except Exception:
                try:
                    self.proc.kill()
                    self.proc.wait(timeout=1.0)
                except Exception:
                    pass
            self.proc = None


# ---------------------------------------------------------------------------
# RTSP Session
# ---------------------------------------------------------------------------

class RTSPSession:
    """Manages a single TCP connection to an RTSP server."""

    def __init__(self, config: RTSPTargetConfig):
        self.config = config
        self.sock: Optional[socket.socket] = None
        self.state = RTSPState.DISCONNECTED
        self.cseq: int = 0
        self.session_id: str = ""
        self.last_response: str = ""
        self.last_code: int = 0
        self.logger = logging.getLogger("rtsp_session")

    @property
    def base_url(self) -> str:
        return f"rtsp://{self.config.host}:{self.config.port}{self.config.stream_path}"

    def connect(self) -> bool:
        """Open TCP connection to the RTSP server."""
        self.close()
        try:
            raw = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
            raw.settimeout(self.config.timeout)
            raw.connect((self.config.host, self.config.port))
            self.sock = raw
            self.state = RTSPState.CONNECTED
            self.cseq = 0
            self.session_id = ""
            self.logger.info("Connected to %s:%d", self.config.host, self.config.port)
            return True
        except (socket.error, OSError) as exc:
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
        self.state = RTSPState.DISCONNECTED

    def _recv(self) -> Optional[str]:
        """Read a complete RTSP response (headers + body)."""
        if self.sock is None:
            return None
        try:
            buf = b""
            # Read until we have complete headers (\r\n\r\n)
            while b"\r\n\r\n" not in buf:
                data = self.sock.recv(4096)
                if not data:
                    return None
                buf += data

            # Check for Content-Length to read body
            header_end = buf.index(b"\r\n\r\n") + 4
            headers = buf[:header_end].decode("utf-8", errors="replace")
            body_so_far = buf[header_end:]

            content_length = 0
            for line in headers.split("\r\n"):
                if line.lower().startswith("content-length:"):
                    try:
                        content_length = int(line.split(":", 1)[1].strip())
                    except ValueError:
                        pass
                    break

            # Read remaining body if needed
            while len(body_so_far) < content_length:
                data = self.sock.recv(4096)
                if not data:
                    break
                body_so_far += data

            full = buf[:header_end] + body_so_far
            resp = full.decode("utf-8", errors="replace")
            self.last_response = resp

            # Extract status code
            first_line = resp.split("\r\n")[0]
            parts = first_line.split()
            if len(parts) >= 2:
                try:
                    self.last_code = int(parts[1])
                except ValueError:
                    self.last_code = 0
            else:
                self.last_code = 0

            # Extract Session header
            for line in resp.split("\r\n"):
                if line.lower().startswith("session:"):
                    sid = line.split(":", 1)[1].strip().split(";")[0]
                    if sid:
                        self.session_id = sid
                    break

            return resp

        except socket.timeout:
            return None
        except (socket.error, OSError):
            return None

    def next_cseq(self) -> int:
        self.cseq += 1
        return self.cseq

    def _build_headers(self, extra_headers: Optional[dict] = None) -> str:
        """Build common headers (CSeq + Session if active)."""
        parts = [f"CSeq: {self.cseq}"]
        if self.session_id:
            parts.append(f"Session: {self.session_id}")
        if extra_headers:
            for k, v in extra_headers.items():
                parts.append(f"{k}: {v}")
        return "\r\n".join(parts)

    def send_request(self, method: str, url: str,
                     extra_headers: Optional[dict] = None,
                     body: str = "") -> Optional[str]:
        """Send a full RTSP request and return the response."""
        if self.sock is None:
            return None

        self.next_cseq()
        headers = self._build_headers(extra_headers)
        if body:
            headers += f"\r\nContent-Length: {len(body)}"

        request = f"{method} {url} RTSP/1.0\r\n{headers}\r\n\r\n"
        if body:
            request += body

        try:
            self.sock.sendall(request.encode("utf-8", errors="replace"))
            return self._recv()
        except (socket.error, OSError, BrokenPipeError):
            return None

    def send_raw(self, data: bytes) -> Optional[str]:
        """Send raw bytes and read RTSP response."""
        if self.sock is None:
            return None
        try:
            self.sock.sendall(data)
            return self._recv()
        except (socket.error, OSError, BrokenPipeError):
            return None


# ---------------------------------------------------------------------------
# Trace parser (same as FTP)
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
# Valid-packet handlers  (protocol setup sequences)
# ---------------------------------------------------------------------------

RTSP_VALID_COMMANDS = {
    0: "OPTIONS",
    1: "DESCRIBE",
    2: "SETUP",
    3: "PLAY",
    4: "PAUSE",
    5: "TEARDOWN",
    6: "GET_PARAMETER",
    7: "SET_PARAMETER",
    8: "ANNOUNCE",
    9: "RECORD",
}


def build_valid_request(pkt_id: int, session: RTSPSession) -> Optional[bytes]:
    """Build a well-formed RTSP request for a valid packet type."""
    method = RTSP_VALID_COMMANDS.get(pkt_id)
    if method is None:
        return None

    cseq = session.next_cseq()
    url = session.base_url
    headers = [f"CSeq: {cseq}"]

    if session.session_id:
        headers.append(f"Session: {session.session_id}")

    if method == "OPTIONS":
        pass  # no extra headers needed
    elif method == "DESCRIBE":
        headers.append("Accept: application/sdp")
    elif method == "SETUP":
        # Use /track1 sub-path for SETUP
        url = session.base_url + "/track1"
        headers.append("Transport: RTP/AVP;unicast;client_port=8000-8001")
    elif method == "PLAY":
        headers.append("Range: npt=0.000-")
    elif method == "GET_PARAMETER":
        pass
    elif method == "SET_PARAMETER":
        pass
    elif method == "ANNOUNCE":
        headers.append("Content-Type: application/sdp")
    elif method == "RECORD":
        headers.append("Range: npt=0.000-")

    header_block = "\r\n".join(headers)
    request = f"{method} {url} RTSP/1.0\r\n{header_block}\r\n\r\n"
    return request.encode("utf-8")


# ---------------------------------------------------------------------------
# Response classification
# ---------------------------------------------------------------------------

def classify_response(
    session: RTSPSession,
    response: Optional[str],
    config: RTSPTargetConfig,
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
    if stripped.startswith("RTSP/"):
        parts = stripped.split(None, 2)
        if len(parts) >= 2:
            try:
                code = int(parts[1])
                if 100 <= code <= 599:
                    return DriverOutput.EXPECTED
            except ValueError:
                pass
    return DriverOutput.UNEXPECTED


def compute_oracle_state(session: RTSPSession) -> int:
    return int(session.state)


# ---------------------------------------------------------------------------
# Sync-file helpers (same as FTP)
# ---------------------------------------------------------------------------

class SyncFiles:
    def __init__(self, sync_dir: str = "sync"):
        self.sync_dir = Path(sync_dir)
        self.message_file = self.sync_dir / "message.txt"
        self.response_file = self.sync_dir / "response.txt"
        self.oracle_file = self.sync_dir / "oracle-response.txt"
        self.score_file = self.sync_dir / "score.txt"

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
        except (OSError, IOError):
            pass

    def write_oracle(self, state: int):
        try:
            self.oracle_file.write_text(str(state))
        except (OSError, IOError):
            pass

    def write_score(self, is_interesting: bool, score: float):
        try:
            label = "interesting" if is_interesting else ""
            self.score_file.write_text(f"{label}\n{score:.6f}\n")
        except (OSError, IOError):
            pass


# ---------------------------------------------------------------------------
# Main driver
# ---------------------------------------------------------------------------

class RTSPFuzzDriver:
    """Main RTSP driver: polls sync/message.txt, replays traces against
    the RTSP target, classifies responses, feeds predicates to the monitor,
    tracks coverage, and writes results back for the fuzzer."""

    def __init__(self, config: RTSPTargetConfig, sync_dir: str = "sync",
                 monitor: Optional[RTSPMonitor] = None,
                 server_config: Optional[ServerConfig] = None):
        self.config = config
        self.sync = SyncFiles(sync_dir)
        self.session = RTSPSession(config)
        self.monitor = monitor
        self.logger = logging.getLogger("driver")
        self.running = True
        self._traces_since_restart = 0

        # Select server manager
        srv = server_config or ServerConfig()
        if srv.use_forkserver and srv.binary:
            srv.port = config.port
            self.server = ForkserverManager(srv)
        else:
            self.server = RTSPProcessManager(srv)

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
        if self.session.state != RTSPState.DISCONNECTED and self.session.sock:
            return True
        return self.session.connect()

    def _reset_connection(self):
        self.session.close()

    def _restart_server_if_due(self):
        if not self.server.managed:
            return
        self._traces_since_restart += 1
        restart_every = self.server.config.restart_every

        need_restart = False
        if not self.server.alive:
            self.logger.warning("Server process died — restarting")
            need_restart = True
        elif restart_every > 0 and self._traces_since_restart >= restart_every:
            need_restart = True

        if need_restart:
            if not self.server.restart():
                time.sleep(1.0)
                self.server.restart()
            self._traces_since_restart = 0
            self.stats["server_restarts"] += 1

    def _feed_request(self, buf: bytes):
        if self.monitor and self.monitor.alive:
            self.monitor.feed_request(buf)
            if self.monitor.check_violation():
                self.stats["violations"] += 1
                self.monitor.clear_violation()

    def _feed_response(self, buf: bytes):
        if self.monitor and self.monitor.alive:
            self.monitor.feed_response(buf)
            if self.monitor.check_violation():
                self.stats["violations"] += 1
                self.monitor.clear_violation()

    def _feed_timeout(self):
        if self.monitor and self.monitor.alive:
            self.monitor.feed_timeout()

    def execute_trace(self, trace: list) -> tuple:
        """Execute a full trace against the RTSP target."""
        self._reset_connection()
        time.sleep(self.config.reconnect_delay)

        if self.monitor:
            self.monitor.adapter.reset_session()

        if not self._ensure_connected():
            return DriverOutput.CRASH, int(RTSPState.DISCONNECTED), None

        last_output = DriverOutput.EXPECTED
        response = None

        for i, (kind, payload) in enumerate(trace):
            if kind == "valid":
                self.logger.debug("  [%d] ValidPacket(%d)", i, payload)
                cmd_bytes = build_valid_request(payload, self.session)
                if cmd_bytes is None:
                    continue

                self._feed_request(cmd_bytes)
                response = self.session.send_raw(cmd_bytes)

                # Update session state based on response
                if response is not None and self.session.last_code >= 200 and self.session.last_code < 300:
                    method = RTSP_VALID_COMMANDS.get(payload, "")
                    if method == "DESCRIBE":
                        self.session.state = RTSPState.DESCRIBED
                    elif method == "SETUP":
                        self.session.state = RTSPState.READY
                    elif method == "PLAY":
                        self.session.state = RTSPState.PLAYING
                    elif method == "RECORD":
                        self.session.state = RTSPState.RECORDING
                    elif method == "PAUSE":
                        self.session.state = RTSPState.READY
                    elif method == "TEARDOWN":
                        self.session.state = RTSPState.CONNECTED

            elif kind == "raw":
                self.logger.debug("  [%d] RawPacket(%d bytes)", i, len(payload))
                self._feed_request(payload)
                response = self.session.send_raw(payload)

            # Feed response/timeout to monitor
            if response is None and self.session.sock is not None:
                self._feed_timeout()
            elif response is not None:
                resp_bytes = response.encode("utf-8", errors="replace")
                self._feed_response(resp_bytes)
            else:
                self._feed_timeout()

            last_output = classify_response(self.session, response, self.config)

            if last_output == DriverOutput.CRASH:
                self.logger.warning("Crash at trace step %d", i)
                break
            if last_output == DriverOutput.TIMEOUT:
                self.logger.debug("Timeout at trace step %d — aborting", i)
                break

        oracle_state = compute_oracle_state(self.session)
        session_result = None

        if self.monitor and self.monitor.alive:
            session_result = self.monitor.end_session()
            if session_result.violated:
                self.stats["violations"] += 1

        self._reset_connection()
        return last_output, oracle_state, session_result

    def run(self):
        """Main polling loop."""
        self._setup_signals()
        self.sync.ensure_files()

        if self.server.managed:
            if not self.server.start():
                self.logger.error("Failed to start RTSP server — aborting")
                return

            if isinstance(self.server, ForkserverManager):
                if not self.server.fork_child():
                    self.logger.error("Failed to fork first child — aborting")
                    return
                self.logger.info("Forkserver: %s", self.server.config.binary)
            else:
                self.logger.info(
                    "Managed server: %s (restart every %d traces)",
                    self.server.config.binary, self.server.config.restart_every,
                )

        if self.monitor:
            self.monitor.start()
            if not self.monitor.alive:
                self.logger.error("Monitor failed to start — continuing without it")
                self.monitor = None

        self.logger.info(
            "RTSP Fuzz Driver started — target %s:%d — polling %s — monitor %s",
            self.config.host, self.config.port,
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

                trace = parse_trace(msg)
                if not trace:
                    self.sync.write_oracle(0)
                    self.sync.write_response(DriverOutput.EXPECTED)
                    self.sync.write_score(False, 0.0)
                    self._restart_server_if_due()
                    continue

                try:
                    output, oracle_state, session_result = self.execute_trace(trace)
                except Exception as exc:
                    self.logger.error("Unhandled exception: %s\n%s", exc, traceback.format_exc())
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

                coverage_score = 0.0
                is_interesting = False
                violated = False
                if session_result is not None:
                    coverage_score = session_result.coverage_score
                    is_interesting = session_result.is_interesting
                    violated = session_result.violated

                if output in (DriverOutput.TIMEOUT, DriverOutput.CRASH):
                    if not violated:
                        is_interesting = False
                        coverage_score = 0.0

                if is_interesting:
                    self.stats["interesting_traces"] += 1
                self.stats["total_coverage_score"] += coverage_score

                self.sync.write_oracle(oracle_state)
                self.sync.write_response(output)
                self.sync.write_score(is_interesting, coverage_score)

                self._restart_server_if_due()

                # Periodic coverage summary
                if trace_num % 100 == 0 and self.monitor:
                    cs = self.monitor.coverage.stats()
                    self.logger.info(
                        "Coverage @ trace %d: %d fps, %d trans, %d rvecs",
                        trace_num, cs.total_fingerprints,
                        cs.total_transitions, cs.total_rule_vectors,
                    )

        finally:
            self.session.close()
            if self.server.managed:
                self.server.stop()
            if self.monitor:
                cs = self.monitor.coverage.stats()
                self.monitor.stop()
                self.logger.info(
                    "Final coverage: %d fps, %d trans, %d rvecs",
                    cs.total_fingerprints, cs.total_transitions,
                    cs.total_rule_vectors,
                )
            self.logger.info("Driver stopped. Stats: %s", json.dumps(self.stats, indent=2))


# ---------------------------------------------------------------------------
# CLI
# ---------------------------------------------------------------------------

def main():
    parser = argparse.ArgumentParser(
        description="RTSP Fuzzing Driver with LTL runtime monitor + predicate coverage"
    )
    parser.add_argument("--host", default="127.0.0.1")
    parser.add_argument("--port", type=int, default=8554)
    parser.add_argument("--timeout", type=float, default=2.0)
    parser.add_argument("--stream-path", default="/test.264",
                        help="RTSP stream path on server")
    parser.add_argument("--sync-dir", default="sync")
    parser.add_argument("--reconnect-delay", type=float, default=0.5)
    parser.add_argument("--poll-interval", type=float, default=0.05)
    parser.add_argument("--log-level", default="INFO",
                        choices=["DEBUG", "INFO", "WARNING", "ERROR"])

    parser.add_argument("--monitor", default=None,
                        help="Path to formula_parser executable")
    parser.add_argument("--spec", default=None,
                        help="Path to RTSP LTL spec file")

    parser.add_argument("--server-binary", default="",
                        help="Path to live555MediaServer binary")
    parser.add_argument("--server-dir", default="",
                        help="Working directory for live555 (where media files are)")
    parser.add_argument("--profraw-dir", default="")
    parser.add_argument("--restart-every", type=int, default=1)
    parser.add_argument("--startup-wait", type=float, default=1.0)
    parser.add_argument("--forkserver", action="store_true")

    args = parser.parse_args()

    logging.basicConfig(
        level=getattr(logging, args.log_level),
        format="%(asctime)s [%(name)s] %(levelname)s: %(message)s",
        datefmt="%H:%M:%S",
    )

    config = RTSPTargetConfig(
        host=args.host,
        port=args.port,
        timeout=args.timeout,
        stream_path=args.stream_path,
        reconnect_delay=args.reconnect_delay,
        poll_interval=args.poll_interval,
    )

    srv_config = None
    if args.server_binary:
        if not Path(args.server_binary).exists():
            logging.getLogger("main").error("Server binary not found: %s", args.server_binary)
            sys.exit(1)
        srv_config = ServerConfig(
            binary=str(Path(args.server_binary).resolve()),
            server_dir=str(Path(args.server_dir).resolve()) if args.server_dir else "",
            profraw_dir=str(Path(args.profraw_dir).resolve()) if args.profraw_dir else "",
            restart_every=args.restart_every,
            startup_wait=args.startup_wait,
            use_forkserver=args.forkserver,
            port=args.port,
        )

    monitor = None
    if args.monitor and args.spec:
        monitor = RTSPMonitor(args.monitor, args.spec)
    elif args.monitor or args.spec:
        logging.getLogger("main").warning(
            "Both --monitor and --spec required to enable monitor"
        )

    driver = RTSPFuzzDriver(config, sync_dir=args.sync_dir, monitor=monitor,
                            server_config=srv_config)
    driver.run()


if __name__ == "__main__":
    main()
