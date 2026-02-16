"""
ftp_monitor.py — Runtime monitor bridge + FTP predicate adapter + coverage tracker.

Ports monitor_bridge.c and ftp_predicate_adapter.c to Python, and adds
predicate-combination coverage as a fuzzing feedback metric.

Coverage model (three layers):
  1. Event fingerprints  — unique abstract predicate vectors (like basic-block coverage)
  2. Transitions         — (prev_fingerprint, curr_fingerprint) pairs (like edge coverage)
  3. Rule subformula vectors — per-rule bitvectors of which violation-condition atoms
                               are simultaneously satisfied (violation proximity)

Usage:
    monitor = FTPMonitor("/path/to/formula_parser", "/path/to/ftp.txt")
    monitor.start()

    monitor.feed_command(b"USER alice\\r\\n")
    monitor.feed_response(b"331 Password required\\r\\n")
    ...

    result = monitor.end_session()
    # result.violated          — LTL property violation detected
    # result.coverage_score    — float score for the fuzzer
    # result.is_interesting    — True if new coverage was discovered
"""

from __future__ import annotations

import os
import select
import subprocess
import sys
from dataclasses import dataclass, field
from typing import Optional


# ============================================================================
# FTP Session State  (mirrors the static struct in ftp_predicate_adapter.c)
# ============================================================================

@dataclass
class FTPSessionState:
    # Authentication
    user_logged_in: bool = False
    user_sent: bool = False
    pass_sent: bool = False
    login_successful: bool = False
    login_failed: bool = False

    # Data connection
    port_sent: bool = False
    pasv_sent: bool = False
    pasv_response_received: bool = False
    port_accepted: bool = False
    data_connection_open: bool = False

    # Transfer
    retr_sent: bool = False
    stor_sent: bool = False
    transfer_started: bool = False
    transfer_complete: bool = False
    transfer_aborted: bool = False
    transfer_in_progress: bool = False

    # Rename
    rnfr_sent: bool = False
    rnfr_accepted: bool = False
    rnto_sent: bool = False

    # Session
    session_initialized: bool = False
    quit_sent: bool = False
    reinit_sent: bool = False
    connection_closed: bool = False

    # Counters
    sequence_number: int = 0
    rest_position: int = 0

    # Last command for state transitions
    last_command: str = "cmdNotSet"


# ============================================================================
# FTP Predicate Adapter  (mirrors ftp_predicate_adapter.c)
# ============================================================================

_FTP_COMMAND_MAP = {
    "USER": "cmdUSER", "PASS": "cmdPASS", "ACCT": "cmdACCT",
    "CWD":  "cmdCWD",  "CDUP": "cmdCDUP", "SMNT": "cmdSMNT",
    "QUIT": "cmdQUIT", "REIN": "cmdREIN", "PORT": "cmdPORT",
    "PASV": "cmdPASV", "TYPE": "cmdTYPE", "STRU": "cmdSTRU",
    "MODE": "cmdMODE", "RETR": "cmdRETR", "STOR": "cmdSTOR",
    "STOU": "cmdSTOU", "APPE": "cmdAPPE", "ALLO": "cmdALLO",
    "REST": "cmdREST", "RNFR": "cmdRNFR", "RNTO": "cmdRNTO",
    "ABOR": "cmdABOR", "DELE": "cmdDELE", "RMD":  "cmdRMD",
    "MKD":  "cmdMKD",  "PWD":  "cmdPWD",  "LIST": "cmdLIST",
    "NLST": "cmdNLST", "SITE": "cmdSITE", "SYST": "cmdSYST",
    "STAT": "cmdSTAT", "HELP": "cmdHELP", "NOOP": "cmdNOOP",
    "FEAT": "cmdFEAT", "OPTS": "cmdOPTS", "AUTH": "cmdAUTH",
    "PBSZ": "cmdPBSZ", "PROT": "cmdPROT", "EPRT": "cmdEPRT",
    "EPSV": "cmdEPSV", "SIZE": "cmdSIZE", "MLSD": "cmdMLSD",
    "MLST": "cmdMLST",
}


def _b(v: bool) -> str:
    return "true" if v else "false"


def _extract_command(buf: bytes) -> str:
    end = 0
    while end < len(buf) and buf[end:end+1] not in (b' ', b'\r', b'\n'):
        end += 1
    return buf[:end].decode("ascii", errors="replace").upper()


def _extract_response_code(buf: bytes) -> int:
    if len(buf) < 3:
        return 0
    try:
        if buf[0:1].isdigit() and buf[1:2].isdigit() and buf[2:3].isdigit():
            return int(buf[0:3])
    except (ValueError, IndexError):
        pass
    return 0


def _map_status_class(code: int) -> str:
    if code == 0:          return "scNotSet"
    if 100 <= code < 200:  return "scPreliminary"
    if 200 <= code < 300:  return "scSuccess"
    if 300 <= code < 400:  return "scIntermediate"
    if 400 <= code < 500:  return "scTransientError"
    if 500 <= code < 600:  return "scPermanentError"
    return "scNotSet"


def _is_cmd_malformed(buf: bytes) -> bool:
    if len(buf) == 0:
        return True
    if len(buf) > 512:
        return True
    if b'\x00' in buf[:-1]:
        return True
    cmd = _extract_command(buf)
    if len(cmd) == 0:
        return True
    return False


def _is_resp_malformed(buf: bytes) -> bool:
    if len(buf) == 0:
        return True
    start = 0
    while start < len(buf):
        end = buf.find(b'\n', start)
        if end < 0:
            end = len(buf)
        line = buf[start:end]
        if line.endswith(b'\r'):
            line = line[:-1]
        if len(line) == 0:
            return True
        if len(line) < 4:
            return True
        if not (line[0:1].isdigit() and line[1:2].isdigit() and line[2:3].isdigit()):
            return True
        start = end + 1 if end < len(buf) else len(buf)
    return False


def _hex_trace(buf: bytes, max_bytes: int = 256) -> str:
    return buf[:max_bytes].hex()


class FTPPredicateAdapter:
    """Mirrors ftp_predicate_adapter.c — maintains session state and builds
    predicate KV lines from raw FTP command/response buffers."""

    def __init__(self):
        self._state = FTPSessionState()
        self._msg_id: int = 0

    def reset_session(self):
        self._state = FTPSessionState()

    def _auth_state(self) -> str:
        s = self._state
        if s.user_logged_in:  return "authComplete"
        if s.login_failed:    return "authFailed"
        if s.pass_sent:       return "authPasswordSent"
        if s.user_sent:       return "authUserSent"
        return "authNone"

    def _data_state(self) -> str:
        s = self._state
        if s.data_connection_open:    return "dataActive"
        if s.pasv_response_received:  return "dataEPSV"
        if s.port_accepted:           return "dataPORT"
        if s.port_sent:               return "dataPORT"
        if s.pasv_sent:               return "dataPASV"
        return "dataNotSet"

    def _infer_command_from_response(self, resp_code: int):
        s = self._state
        if s.last_command != "cmdNotSet":
            return
        if resp_code == 220:
            if s.sequence_number == 0:
                pass
            elif s.reinit_sent:
                s.last_command = "cmdREIN"
        elif resp_code == 331:
            if s.user_sent:
                s.last_command = "cmdUSER"
        elif resp_code == 230:
            if s.sequence_number == 0:
                pass
            elif s.pass_sent:
                s.last_command = "cmdPASS"
        elif resp_code == 227:
            if s.pasv_sent:
                s.last_command = "cmdPASV"
        elif resp_code == 350:
            if s.rnfr_sent:
                s.last_command = "cmdRNFR"
        elif resp_code == 221:
            if s.quit_sent:
                s.last_command = "cmdQUIT"
        elif resp_code == 257:
            s.last_command = "cmdPWD"
        elif resp_code == 215:
            s.last_command = "cmdSYST"
        elif resp_code == 211:
            s.last_command = "cmdFEAT"

    def _format_pred_line(
        self,
        cmd_enum: str,
        status_class: str,
        resp_code: int,
        cmd_malformed: bool,
        resp_malformed: bool,
        timeout: bool,
        port_number: int = 0,
        file_size: int = 0,
    ) -> str:
        s = self._state
        parts = [
            f"ftp_command={cmd_enum}",
            f"ftp_status_class={status_class}",
            f"resp_code={resp_code}",
            f"sequence_number={s.sequence_number}",
            f"port_number={port_number}",
            f"file_size={file_size}",
            f"rest_position={s.rest_position}",
            f"cmd_malformed={_b(cmd_malformed)}",
            f"resp_malformed={_b(resp_malformed)}",
            f"user_logged_in={_b(s.user_logged_in)}",
            f"data_connection_open={_b(s.data_connection_open)}",
            f"transfer_in_progress={_b(s.transfer_in_progress)}",
            f"timeout={_b(timeout)}",
            f"connection_closed={_b(s.connection_closed)}",
            f"user_sent={_b(s.user_sent)}",
            f"pass_sent={_b(s.pass_sent)}",
            f"login_successful={_b(s.login_successful)}",
            f"login_failed={_b(s.login_failed)}",
            f"port_sent={_b(s.port_sent)}",
            f"pasv_sent={_b(s.pasv_sent)}",
            f"pasv_response_received={_b(s.pasv_response_received)}",
            f"port_accepted={_b(s.port_accepted)}",
            f"retr_sent={_b(s.retr_sent)}",
            f"stor_sent={_b(s.stor_sent)}",
            f"transfer_started={_b(s.transfer_started)}",
            f"transfer_complete={_b(s.transfer_complete)}",
            f"transfer_aborted={_b(s.transfer_aborted)}",
            f"rnfr_sent={_b(s.rnfr_sent)}",
            f"rnfr_accepted={_b(s.rnfr_accepted)}",
            f"rnto_sent={_b(s.rnto_sent)}",
            f"session_initialized={_b(s.session_initialized)}",
            f"quit_sent={_b(s.quit_sent)}",
            f"reinit_sent={_b(s.reinit_sent)}",
            f"auth_state={self._auth_state()}",
            f"data_state={self._data_state()}",
            "transfer_type=typeNotSet",
        ]
        return " ".join(parts)

    def build_command_pred_line(self, buf: bytes) -> str:
        s = self._state
        cmd_str = _extract_command(buf)
        cmd_enum = _FTP_COMMAND_MAP.get(cmd_str, "cmdNotSet")
        s.last_command = cmd_enum

        if cmd_enum == "cmdUSER":
            s.user_sent = True
        elif cmd_enum == "cmdPASS":
            s.pass_sent = True
        elif cmd_enum == "cmdPORT":
            s.port_sent = True
            s.pasv_sent = False
        elif cmd_enum == "cmdPASV":
            s.pasv_sent = True
            s.port_sent = False
        elif cmd_enum == "cmdRETR":
            s.retr_sent = True
        elif cmd_enum == "cmdSTOR":
            s.stor_sent = True
        elif cmd_enum == "cmdRNFR":
            s.rnfr_sent = True
        elif cmd_enum == "cmdRNTO":
            s.rnto_sent = True
        elif cmd_enum == "cmdABOR":
            s.transfer_aborted = True
            s.transfer_in_progress = False
        elif cmd_enum == "cmdQUIT":
            s.quit_sent = True
        elif cmd_enum == "cmdREIN":
            s.reinit_sent = True
        elif cmd_enum == "cmdREST":
            rest_str = buf[len(cmd_str):].lstrip()
            try:
                s.rest_position = int(rest_str.split()[0])
            except (ValueError, IndexError):
                pass

        s.sequence_number += 1
        cmd_malformed = _is_cmd_malformed(buf)

        self._msg_id += 1
        line = self._format_pred_line(
            cmd_enum=cmd_enum, status_class="scNotSet", resp_code=0,
            cmd_malformed=cmd_malformed, resp_malformed=False, timeout=False,
        )
        line += f" msg_id={self._msg_id} dir=C2S trace={_hex_trace(buf)}"
        return line

    def build_response_pred_line(self, buf: bytes) -> str:
        s = self._state
        resp_code = _extract_response_code(buf)
        status_class = _map_status_class(resp_code)
        resp_malformed = _is_resp_malformed(buf)
        timeout = (len(buf) == 0 or resp_code == 0)

        self._infer_command_from_response(resp_code)

        if resp_code == 220 and s.sequence_number == 0:
            s.session_initialized = True
        elif resp_code == 230:
            s.user_logged_in = True
            s.login_successful = True
        elif resp_code == 530:
            s.login_failed = True
            s.user_logged_in = False
        elif resp_code == 200 and s.last_command == "cmdPORT":
            s.port_accepted = True
        elif resp_code == 227:
            s.pasv_response_received = True
        elif resp_code == 150:
            s.transfer_started = True
            s.transfer_in_progress = True
            s.data_connection_open = True
        elif resp_code == 226:
            s.transfer_complete = True
            s.transfer_in_progress = False
            s.data_connection_open = False
        elif resp_code == 350 and s.last_command == "cmdRNFR":
            s.rnfr_accepted = True
        elif resp_code == 220 and s.last_command == "cmdREIN":
            self.reset_session()
            s = self._state
            s.session_initialized = True
        elif resp_code == 221:
            s.connection_closed = True

        s.sequence_number += 1

        self._msg_id += 1
        line = self._format_pred_line(
            cmd_enum=s.last_command, status_class=status_class,
            resp_code=resp_code, cmd_malformed=False,
            resp_malformed=resp_malformed, timeout=timeout,
        )
        line += f" msg_id={self._msg_id} dir=S2C trace={_hex_trace(buf)}"
        return line

    def build_timeout_pred_line(self) -> str:
        s = self._state
        s.sequence_number += 1
        self._msg_id += 1
        line = self._format_pred_line(
            cmd_enum=s.last_command, status_class="scNotSet", resp_code=0,
            cmd_malformed=False, resp_malformed=False, timeout=True,
        )
        line += f" msg_id={self._msg_id} dir=S2C trace="
        return line


# ============================================================================
# Monitor Bridge  (mirrors monitor_bridge.c)
# ============================================================================

class MonitorBridge:
    """Launches formula_parser as a subprocess and communicates via pipes."""

    def __init__(self, eval_path: str, spec_path: str, protocol_tag: str = "ftp"):
        self._eval_path = eval_path
        self._spec_path = spec_path
        self._protocol_tag = protocol_tag
        self._proc: Optional[subprocess.Popen] = None
        self.violation_detected: bool = False

    def start(self):
        self._proc = subprocess.Popen(
            [self._eval_path, self._spec_path, self._protocol_tag],
            stdin=subprocess.PIPE, stdout=subprocess.PIPE,
            stderr=None, bufsize=0,
        )

    def stop(self) -> int:
        if self._proc is None:
            return -1
        if self._proc.stdin:
            try:
                self._proc.stdin.close()
            except OSError:
                pass
        if self._proc.stdout:
            try:
                self._proc.stdout.close()
            except OSError:
                pass
        self._proc.wait()
        code = self._proc.returncode
        self._proc = None
        return code

    @property
    def alive(self) -> bool:
        return self._proc is not None and self._proc.poll() is None

    def emit_line(self, line: str):
        if self._proc is None or self._proc.stdin is None:
            return
        self._proc.stdin.write((line + "\n").encode("utf-8"))
        self._proc.stdin.flush()

    def _drain_stdout(self, timeout_sec: float = 0.01):
        if self._proc is None or self._proc.stdout is None:
            return
        fd = self._proc.stdout.fileno()
        while True:
            rlist, _, _ = select.select([fd], [], [], timeout_sec)
            if not rlist:
                break
            chunk = os.read(fd, 4096)
            if not chunk:
                break
            text = chunk.decode("utf-8", errors="replace")
            if "VIOLATION_DETECTED" in text:
                self.violation_detected = True
            timeout_sec = 0.0

    def end_session(self) -> bool:
        self.emit_line("__END_SESSION__")
        self._drain_stdout(timeout_sec=0.01)
        violated = self.violation_detected
        self.violation_detected = False
        return violated

    def save_state(self, snapshot_id: int):
        self.emit_line(f"__SAVE_STATE__ {snapshot_id}")
        self._drain_stdout(timeout_sec=0.05)

    def restore_state(self, snapshot_id: int):
        self.emit_line(f"__RESTORE_STATE__ {snapshot_id}")
        self._drain_stdout(timeout_sec=0.05)


# ============================================================================
# Predicate Coverage Tracker
# ============================================================================

from spec_parser import (
    ParsedSpec, CoverageRule, RuleKind, parse_spec_file,
)


# ---------------------------------------------------------------------------
# KV line parsing
# ---------------------------------------------------------------------------

def _parse_kv_line(line: str) -> dict:
    """Parse 'key=value key=value ...' into a dict."""
    kv = {}
    for token in line.split():
        eq = token.find('=')
        if eq > 0:
            kv[token[:eq]] = token[eq + 1:]
    return kv


# ---------------------------------------------------------------------------
# Fingerprint: derived from spec variable declarations
# ---------------------------------------------------------------------------
# Enums and booleans are kept as-is (categorical).
# Ints are bucketed: resp_code is kept exact (bounded domain, security-
# relevant distinctions); all others bucket to {0, positive}.
# Metadata keys (msg_id, dir, trace, sequence_number) are excluded.

_METADATA_KEYS = {'msg_id', 'dir', 'trace', 'sequence_number'}
_EXACT_INT_KEYS = {'resp_code'}


def _build_fingerprint_keys(spec: ParsedSpec) -> tuple[list[str], list[str], list[str]]:
    """Derive fingerprint key lists from spec declarations.
    Returns (exact_keys, bucket_keys, enum_keys)."""
    exact_keys = []    # included with exact value
    bucket_keys = []   # bucketed to {0, positive}
    for vtype, vname in spec.variables:
        if vname in _METADATA_KEYS:
            continue
        if vtype == 'int':
            if vname in _EXACT_INT_KEYS:
                exact_keys.append(vname)
            else:
                bucket_keys.append(vname)
        else:
            exact_keys.append(vname)
    # Add enum-typed variables (auth_state, data_state, etc.)
    for ename in spec.enums:
        exact_keys.append(ename)
    return exact_keys, bucket_keys


def _fingerprint(kv: dict, exact_keys: list[str], bucket_keys: list[str]) -> tuple:
    """Extract a coverage fingerprint from a parsed KV dict."""
    vals = [kv.get(k, '') for k in exact_keys]
    for k in bucket_keys:
        try:
            vals.append('pos' if int(kv.get(k, '0')) > 0 else '0')
        except ValueError:
            vals.append('0')
    return tuple(vals)


# ---------------------------------------------------------------------------
# Rule evaluation helpers
# ---------------------------------------------------------------------------

def _eval_atoms(kv: dict, atoms) -> tuple:
    """Evaluate a list of CoverageAtoms, return bitvector as tuple of 0/1."""
    vec = []
    for atom in atoms:
        try:
            vec.append(1 if atom.evaluator(kv) else 0)
        except (ValueError, TypeError, KeyError):
            vec.append(0)
    return tuple(vec)


# ---------------------------------------------------------------------------
# Scoring weights
# ---------------------------------------------------------------------------

WEIGHT_NEW_FINGERPRINT  = 1.0    # new abstract state seen
WEIGHT_NEW_TRANSITION   = 2.0    # new state transition (edge coverage)
WEIGHT_NEW_RULE_VECTOR  = 3.0    # new subformula combination for a rule
WEIGHT_PROXIMITY_STEP   = 10.0   # one atom closer to activating a rule's condition
WEIGHT_ANTECEDENT_ACTIVE = 20.0  # implication antecedent fully activated for first time
WEIGHT_CONSEQUENT_FALSIFIED = 50.0  # Q=false observed while P active (violation boundary)


# ---------------------------------------------------------------------------
# Session result
# ---------------------------------------------------------------------------

@dataclass
class SessionResult:
    """Returned by FTPMonitor.end_session()."""
    violated: bool = False
    coverage_score: float = 0.0
    is_interesting: bool = False
    new_fingerprints: int = 0
    new_transitions: int = 0
    new_rule_vectors: int = 0
    proximity_steps: int = 0


# ---------------------------------------------------------------------------
# Coverage statistics (global, read-only snapshot)
# ---------------------------------------------------------------------------

@dataclass
class CoverageStats:
    """Snapshot of global coverage state for logging/debugging."""
    total_fingerprints: int = 0
    total_transitions: int = 0
    total_rule_vectors: int = 0
    max_proximity: dict = field(default_factory=dict)
    # max_proximity[rule_name] = max atoms simultaneously satisfied
    # For implications: (max_P_atoms, P_ever_active, Q_falsified_ever)


# ---------------------------------------------------------------------------
# Tracker
# ---------------------------------------------------------------------------

class PredicateCoverageTracker:
    """Tracks predicate-combination coverage across all fuzzing traces.

    Three coverage layers, with rule-kind-aware evaluation:

    1. Event fingerprints — unique abstract predicate vectors.
    2. Transitions — (prev_fingerprint, curr_fingerprint) pairs.
    3. Rule subformula coverage:
       - CONJUNCTION/INVARIANT: flat bitvector, proximity = count of true atoms.
       - IMPLICATION (P -> Q): two-tier.
         Tier 1: P-bitvector always recorded; proximity tracks P-activation.
         Tier 2: Q-observation recorded ONLY when P is fully satisfied.
         This prevents false proximity from inactive antecedents and
         correctly values Q-falsification as a rare, high-signal event.
    """

    def __init__(self, spec: ParsedSpec):
        self._spec = spec
        self._rules = spec.rules

        # Derive fingerprint keys from spec
        self._fp_exact, self._fp_bucket = _build_fingerprint_keys(spec)

        # ---- Global state (persists across all traces) ----
        self._seen_fps: set[int] = set()
        self._seen_trans: set[tuple[int, int]] = set()

        # Per-rule global state
        # Conjunction/Invariant: {(rule_idx, bitvec)} and max proximity
        # Implication: {(rule_idx, 'P', p_bitvec)}, {(rule_idx, 'Q', q_bitvec)}
        #   where Q-vectors are only recorded when P is fully active.
        self._seen_rule_vecs: set[tuple] = set()
        self._max_proximity: list[int] = []   # one per rule
        self._p_ever_active: list[bool] = []  # implication only: P reached all-true
        self._q_ever_false: list[bool] = []   # implication only: Q=false while P active

        for rule in self._rules:
            if rule.kind == RuleKind.IMPLICATION:
                self._max_proximity.append(0)  # tracks P-atom proximity
                self._p_ever_active.append(False)
                self._q_ever_false.append(False)
            else:
                n_atoms = len(rule.atoms)
                self._max_proximity.append(0)
                self._p_ever_active.append(False)   # unused for non-implications
                self._q_ever_false.append(False)     # unused for non-implications

        # ---- Per-trace state (reset each trace) ----
        self._prev_fp_hash: Optional[int] = None
        self._trace_new_fps: int = 0
        self._trace_new_trans: int = 0
        self._trace_new_rvecs: int = 0
        self._trace_prox_steps: int = 0

    def record_event(self, kv_line: str):
        """Record a single predicate event."""
        kv = _parse_kv_line(kv_line)

        # --- Layer 1: event fingerprint ---
        fp = _fingerprint(kv, self._fp_exact, self._fp_bucket)
        fp_hash = hash(fp)
        if fp_hash not in self._seen_fps:
            self._seen_fps.add(fp_hash)
            self._trace_new_fps += 1

        # --- Layer 2: transition ---
        if self._prev_fp_hash is not None:
            trans = (self._prev_fp_hash, fp_hash)
            if trans not in self._seen_trans:
                self._seen_trans.add(trans)
                self._trace_new_trans += 1
        self._prev_fp_hash = fp_hash

        # --- Layer 3: rule subformula coverage ---
        for rule_idx, rule in enumerate(self._rules):
            if rule.kind == RuleKind.IMPLICATION:
                self._record_implication(rule_idx, rule, kv)
            else:
                self._record_conjunction(rule_idx, rule, kv)

    def _record_conjunction(self, rule_idx: int, rule: CoverageRule, kv: dict):
        """Flat bitvector tracking for CONJUNCTION and INVARIANT rules."""
        vec = _eval_atoms(kv, rule.atoms)
        key = (rule_idx, vec)
        if key not in self._seen_rule_vecs:
            self._seen_rule_vecs.add(key)
            self._trace_new_rvecs += 1

        n_sat = sum(vec)
        if n_sat > self._max_proximity[rule_idx]:
            self._trace_prox_steps += n_sat - self._max_proximity[rule_idx]
            self._max_proximity[rule_idx] = n_sat

    def _record_implication(self, rule_idx: int, rule: CoverageRule, kv: dict):
        """Two-tier tracking for IMPLICATION rules (P -> Q).

        Tier 1: Always record the P-bitvector and track P-proximity.
        Tier 2: Only record Q-observation when ALL P atoms are satisfied.
        """
        p_vec = _eval_atoms(kv, rule.antecedent_atoms)

        # Tier 1: P-bitvector (always recorded)
        p_key = (rule_idx, 'P', p_vec)
        if p_key not in self._seen_rule_vecs:
            self._seen_rule_vecs.add(p_key)
            self._trace_new_rvecs += 1

        p_sat = sum(p_vec)
        if p_sat > self._max_proximity[rule_idx]:
            self._trace_prox_steps += p_sat - self._max_proximity[rule_idx]
            self._max_proximity[rule_idx] = p_sat

        # Tier 2: Q-observation (only when P fully active)
        p_fully_active = all(v == 1 for v in p_vec)
        if not p_fully_active:
            return

        # P just became fully active — bonus if first time ever
        if not self._p_ever_active[rule_idx]:
            self._p_ever_active[rule_idx] = True
            self._trace_prox_steps += 1  # scored via WEIGHT_ANTECEDENT_ACTIVE below

        q_vec = _eval_atoms(kv, rule.consequent_atoms)
        q_key = (rule_idx, 'Q', q_vec)
        if q_key not in self._seen_rule_vecs:
            self._seen_rule_vecs.add(q_key)
            self._trace_new_rvecs += 1

        # Q fully falsified while P active — violation boundary
        q_all_false = all(v == 0 for v in q_vec)
        if q_all_false and not self._q_ever_false[rule_idx]:
            self._q_ever_false[rule_idx] = True
            self._trace_prox_steps += 1  # scored via WEIGHT_CONSEQUENT_FALSIFIED below

    def end_trace(self) -> tuple[float, bool]:
        """Finalize the current trace. Returns (score, is_interesting)."""
        score = (
            self._trace_new_fps   * WEIGHT_NEW_FINGERPRINT +
            self._trace_new_trans * WEIGHT_NEW_TRANSITION +
            self._trace_new_rvecs * WEIGHT_NEW_RULE_VECTOR +
            self._trace_prox_steps * WEIGHT_PROXIMITY_STEP
        )
        is_interesting = (
            self._trace_new_fps > 0 or
            self._trace_new_trans > 0 or
            self._trace_new_rvecs > 0
        )
        self._last_new_fps = self._trace_new_fps
        self._last_new_trans = self._trace_new_trans
        self._last_new_rvecs = self._trace_new_rvecs
        self._last_prox_steps = self._trace_prox_steps

        self._prev_fp_hash = None
        self._trace_new_fps = 0
        self._trace_new_trans = 0
        self._trace_new_rvecs = 0
        self._trace_prox_steps = 0

        return score, is_interesting

    def stats(self) -> CoverageStats:
        """Return a snapshot of global coverage statistics."""
        proximity = {}
        for i, rule in enumerate(self._rules):
            if rule.kind == RuleKind.IMPLICATION:
                n_p = len(rule.antecedent_atoms)
                proximity[rule.name] = (
                    f"{self._max_proximity[i]}/{n_p}P"
                    f" active={self._p_ever_active[i]}"
                    f" Q_false={self._q_ever_false[i]}"
                )
            else:
                n = len(rule.atoms)
                proximity[rule.name] = f"{self._max_proximity[i]}/{n}"

        return CoverageStats(
            total_fingerprints=len(self._seen_fps),
            total_transitions=len(self._seen_trans),
            total_rule_vectors=len(self._seen_rule_vecs),
            max_proximity=proximity,
        )


# ============================================================================
# FTPMonitor — Combined high-level API
# ============================================================================

class FTPMonitor:
    """All-in-one: launches the monitor, translates raw FTP buffers into
    predicate lines, feeds them to the evaluator, and tracks coverage.

    This is the class the Python fuzzer driver should instantiate.
    """

    def __init__(self, eval_path: str, spec_path: str):
        self.bridge = MonitorBridge(eval_path, spec_path, protocol_tag="ftp")
        self.adapter = FTPPredicateAdapter()
        self._spec = parse_spec_file(spec_path)
        self.coverage = PredicateCoverageTracker(self._spec)

    def start(self):
        self.bridge.start()

    def stop(self) -> int:
        return self.bridge.stop()

    @property
    def alive(self) -> bool:
        return self.bridge.alive

    # ------------------------------------------------------------------
    #  Feed raw bytes  (the driver calls these)
    # ------------------------------------------------------------------

    def feed_command(self, buf: bytes):
        """Translate a raw FTP command, emit the predicate line, record coverage."""
        line = self.adapter.build_command_pred_line(buf)
        self.bridge.emit_line(line)
        self.coverage.record_event(line)

    def feed_response(self, buf: bytes):
        """Translate a raw FTP response, emit the predicate line, record coverage."""
        line = self.adapter.build_response_pred_line(buf)
        self.bridge.emit_line(line)
        self.coverage.record_event(line)

    def feed_timeout(self):
        """Emit a timeout predicate line and record coverage."""
        line = self.adapter.build_timeout_pred_line()
        self.bridge.emit_line(line)
        self.coverage.record_event(line)

    # ------------------------------------------------------------------
    #  Session / snapshot control
    # ------------------------------------------------------------------

    def end_session(self) -> SessionResult:
        """End the current session. Returns a SessionResult with violation
        status and coverage metrics."""
        violated = self.bridge.end_session()
        score, is_interesting = self.coverage.end_trace()
        self.adapter.reset_session()

        return SessionResult(
            violated=violated,
            coverage_score=score,
            is_interesting=is_interesting,
            new_fingerprints=self.coverage._last_new_fps,
            new_transitions=self.coverage._last_new_trans,
            new_rule_vectors=self.coverage._last_new_rvecs,
            proximity_steps=self.coverage._last_prox_steps,
        )

    def save_state(self, snapshot_id: int):
        self.bridge.save_state(snapshot_id)

    def restore_state(self, snapshot_id: int):
        self.bridge.restore_state(snapshot_id)

    # ------------------------------------------------------------------
    #  Direct violation check (without ending session)
    # ------------------------------------------------------------------

    def check_violation(self) -> bool:
        self.bridge._drain_stdout(timeout_sec=0.0)
        return self.bridge.violation_detected

    def clear_violation(self):
        self.bridge.violation_detected = False


# ============================================================================
# Quick self-test
# ============================================================================

if __name__ == "__main__":
    import argparse as _ap
    import sys
    from spec_parser import parse_spec, parse_spec_file, RuleKind

    _parser = _ap.ArgumentParser(description="ftp_monitor coverage tracker self-test")
    _parser.add_argument("--spec", default=None,
                         help="Path to spec file (omit for built-in unit test)")
    _args = _parser.parse_args()

    # ------------------------------------------------------------------
    # Load spec: either from file or inline
    # ------------------------------------------------------------------
    if _args.spec:
        print(f"=== Loading spec from {_args.spec} ===\n")
        spec = parse_spec_file(_args.spec)
    else:
        _TEST_SPEC = """
enum ftp_command { cmdNotSet, cmdUSER, cmdPASS, cmdRETR, cmdSTOR, cmdDELE, cmdRNFR, cmdRNTO, cmdPASV, cmdPORT, cmdQUIT };
enum ftp_status_class { scNotSet, scPreliminary, scSuccess, scIntermediate, scTransientError, scPermanentError };
enum auth_state { authNone, authUserSent, authPasswordSent, authComplete, authFailed };
enum data_state { dataNotSet, dataPORT, dataPASV, dataEPRT, dataEPSV, dataActive, dataClosed };
enum transfer_type { typeNotSet, typeASCII, typeBinary, typeEBCDIC, typeLocal };
int resp_code;
int sequence_number;
int port_number;
int file_size;
int rest_position;
bool cmd_malformed;
bool resp_malformed;
bool user_logged_in;
bool data_connection_open;
bool transfer_in_progress;
bool timeout;
bool connection_closed;
bool user_sent;
bool pass_sent;
bool login_successful;
bool login_failed;
bool port_sent;
bool pasv_sent;
bool pasv_response_received;
bool port_accepted;
bool retr_sent;
bool stor_sent;
bool transfer_started;
bool transfer_complete;
bool transfer_aborted;
bool rnfr_sent;
bool rnfr_accepted;
bool rnto_sent;
bool session_initialized;
bool quit_sent;
bool reinit_sent;

// CONJUNCTION: auth bypass
H(!(timeout=false & resp_malformed=false & connection_closed=false & quit_sent=false & ftp_command=cmdPASS & resp_code=230 & user_sent=false));

// INVARIANT: no malformed responses
H(resp_malformed=false);

// IMPLICATION: 227 must come from PASV
H((timeout=false & resp_malformed=false & connection_closed=false & quit_sent=false & resp_code=227) -> ftp_command=cmdPASV);

// IMPLICATION: PORT success requires valid port
H((timeout=false & connection_closed=false & quit_sent=false & ftp_command=cmdPORT & ftp_status_class=scSuccess) -> port_number>0);
"""
        print("=== Using built-in test spec ===\n")
        spec = parse_spec(_TEST_SPEC)

    tracker = PredicateCoverageTracker(spec)
    adapter = FTPPredicateAdapter()

    # ------------------------------------------------------------------
    # Spec summary
    # ------------------------------------------------------------------
    conj_rules = [(i, r) for i, r in enumerate(spec.rules) if r.kind == RuleKind.CONJUNCTION]
    impl_rules = [(i, r) for i, r in enumerate(spec.rules) if r.kind == RuleKind.IMPLICATION]
    inv_rules  = [(i, r) for i, r in enumerate(spec.rules) if r.kind == RuleKind.INVARIANT]

    print(f"Parsed {len(spec.rules)} rules "
          f"({len(conj_rules)} conjunction, {len(impl_rules)} implication, {len(inv_rules)} invariant):")
    for r in spec.rules:
        print(f"  {r.name} ({r.kind.name}): {r.source[:80]}{'...' if len(r.source) > 80 else ''}")
    print(f"\nEnums: {len(spec.enums)}  Variables: {len(spec.variables)}")
    print()

    # ------------------------------------------------------------------
    # Trace 1: Normal login — baseline coverage
    # ------------------------------------------------------------------
    events = [
        adapter.build_response_pred_line(b"220 Welcome\r\n"),
        adapter.build_command_pred_line(b"USER alice\r\n"),
        adapter.build_response_pred_line(b"331 Password required\r\n"),
        adapter.build_command_pred_line(b"PASS secret\r\n"),
        adapter.build_response_pred_line(b"230 Login successful\r\n"),
        adapter.build_command_pred_line(b"QUIT\r\n"),
        adapter.build_response_pred_line(b"221 Goodbye\r\n"),
    ]
    for e in events:
        tracker.record_event(e)
    score1, interesting1 = tracker.end_trace()
    s1 = tracker.stats()
    adapter.reset_session()

    print(f"Trace 1 (normal login): score={score1:.1f} interesting={interesting1}")
    print(f"  Coverage: {s1.total_fingerprints} fps, {s1.total_transitions} trans, "
          f"{s1.total_rule_vectors} rvecs")
    print(f"  Proximity: {s1.max_proximity}")
    assert interesting1, "First trace should always be interesting"
    assert s1.total_fingerprints > 0 and s1.total_transitions > 0

    # Structural check: no implication antecedent should be active from a normal login
    for idx, rule in impl_rules:
        assert not tracker._p_ever_active[idx], (
            f"{rule.name}: P should not be active after normal login")
    print("  [OK] Implication antecedents correctly inactive")

    # ------------------------------------------------------------------
    # Trace 2: Exact repeat — perfect dedup
    # ------------------------------------------------------------------
    for e in list(events):
        tracker.record_event(e)
    score2, interesting2 = tracker.end_trace()
    adapter.reset_session()

    print(f"\nTrace 2 (repeat): score={score2:.1f} interesting={interesting2}")
    assert score2 == 0.0 and not interesting2, "Repeated trace should be uninteresting"
    print("  [OK] Perfect dedup")

    # ------------------------------------------------------------------
    # Trace 3: Auth bypass — PASS without USER, gets 230
    # ------------------------------------------------------------------
    events3 = [
        adapter.build_response_pred_line(b"220 Welcome\r\n"),
        adapter.build_command_pred_line(b"PASS hacked\r\n"),
        adapter.build_response_pred_line(b"230 Login successful\r\n"),
    ]
    for e in events3:
        tracker.record_event(e)
    score3, interesting3 = tracker.end_trace()
    s3 = tracker.stats()
    adapter.reset_session()

    print(f"\nTrace 3 (auth bypass): score={score3:.1f} interesting={interesting3}")
    print(f"  Proximity: {s3.max_proximity}")
    assert interesting3, "Auth bypass trace should be interesting"

    # If there are conjunction rules, the first one (auth bypass) should have
    # high proximity — all atoms except maybe the one distinguishing the test
    if conj_rules:
        conj_idx, conj_rule = conj_rules[0]
        n_atoms = len(conj_rule.atoms)
        prox = tracker._max_proximity[conj_idx]
        print(f"  [OK] Conjunction {conj_rule.name}: proximity {prox}/{n_atoms}")
        assert prox >= n_atoms - 1, (
            f"Auth bypass should reach at least {n_atoms-1}/{n_atoms} atoms, got {prox}")

    # ------------------------------------------------------------------
    # Trace 4: PASV → 227 (implication: P active, Q holds)
    # ------------------------------------------------------------------
    events4 = [
        adapter.build_response_pred_line(b"220 Welcome\r\n"),
        adapter.build_command_pred_line(b"PASV\r\n"),
        adapter.build_response_pred_line(b"227 Entering Passive Mode\r\n"),
    ]
    for e in events4:
        tracker.record_event(e)
    score4, interesting4 = tracker.end_trace()
    s4 = tracker.stats()
    adapter.reset_session()

    print(f"\nTrace 4 (PASV->227, Q holds): score={score4:.1f} interesting={interesting4}")
    print(f"  Proximity: {s4.max_proximity}")

    # Find the 227 implication rule (if present in this spec)
    pasv_impl = None
    for idx, rule in impl_rules:
        if any('227' in a.label for a in rule.antecedent_atoms):
            pasv_impl = (idx, rule)
            break

    if pasv_impl:
        pidx, prule = pasv_impl
        assert tracker._p_ever_active[pidx], (
            f"{prule.name}: P should be active (resp_code=227 seen)")
        assert not tracker._q_ever_false[pidx], (
            f"{prule.name}: Q should NOT be false (ftp_command was cmdPASV)")
        print(f"  [OK] {prule.name}: P active, Q holds (no violation)")
    else:
        print("  [SKIP] No 227 implication rule in this spec")

    # ------------------------------------------------------------------
    # Trace 5: LIST → 227 (implication: P active, Q FALSIFIED)
    # ------------------------------------------------------------------
    events5 = [
        adapter.build_response_pred_line(b"220 Welcome\r\n"),
        adapter.build_command_pred_line(b"LIST\r\n"),
        adapter.build_response_pred_line(b"227 Entering Passive Mode\r\n"),
    ]
    for e in events5:
        tracker.record_event(e)
    score5, interesting5 = tracker.end_trace()
    s5 = tracker.stats()
    adapter.reset_session()

    print(f"\nTrace 5 (LIST->227, Q falsified): score={score5:.1f} interesting={interesting5}")
    print(f"  Proximity: {s5.max_proximity}")

    if pasv_impl:
        pidx, prule = pasv_impl
        assert interesting5, "Q-falsification should be interesting"
        assert tracker._q_ever_false[pidx], (
            f"{prule.name}: Q should be false (ftp_command was cmdLIST, not cmdPASV)")
        print(f"  [OK] {prule.name}: Q falsified — violation boundary reached")
    else:
        print("  [SKIP] No 227 implication rule in this spec")

    # ------------------------------------------------------------------
    # Trace 6: LIST → 150 (P inactive, Q suppressed)
    # ------------------------------------------------------------------
    pre_rvecs = tracker.stats().total_rule_vectors
    events6 = [
        adapter.build_response_pred_line(b"220 Welcome\r\n"),
        adapter.build_command_pred_line(b"LIST\r\n"),
        adapter.build_response_pred_line(b"150 Opening data connection\r\n"),
    ]
    for e in events6:
        tracker.record_event(e)
    score6, interesting6 = tracker.end_trace()
    s6 = tracker.stats()
    adapter.reset_session()

    print(f"\nTrace 6 (LIST->150, P inactive): score={score6:.1f} interesting={interesting6}")
    print(f"  Rule vectors: {pre_rvecs} -> {s6.total_rule_vectors}")
    print(f"  [OK] P inactive — Q observation correctly suppressed")

    # ------------------------------------------------------------------
    # Final summary
    # ------------------------------------------------------------------
    print(f"\n=== Final coverage state ===")
    final = tracker.stats()
    print(f"  Fingerprints: {final.total_fingerprints}")
    print(f"  Transitions:  {final.total_transitions}")
    print(f"  Rule vectors: {final.total_rule_vectors}")
    print(f"  Proximity:")
    for name, val in final.max_proximity.items():
        print(f"    {name}: {val}")

    print("\n=== All assertions passed ===")