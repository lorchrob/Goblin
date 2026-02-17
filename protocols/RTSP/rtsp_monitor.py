"""
rtsp_monitor.py — Runtime monitor bridge + RTSP predicate adapter + coverage tracker.

Mirrors the architecture of ftp_monitor.py for RTSP (RFC 2326 / RFC 7826).

Coverage model (three layers):
  1. Event fingerprints  — unique abstract predicate vectors
  2. Transitions         — (prev_fingerprint, curr_fingerprint) pairs
  3. Rule subformula vectors — per-rule bitvectors of which atoms are satisfied

Usage:
    monitor = RTSPMonitor("/path/to/formula_parser", "/path/to/rtsp.txt")
    monitor.start()

    monitor.feed_request(b"DESCRIBE rtsp://127.0.0.1/test.264 RTSP/1.0\\r\\nCSeq: 1\\r\\n\\r\\n")
    monitor.feed_response(b"RTSP/1.0 200 OK\\r\\nCSeq: 1\\r\\n\\r\\n")
    ...

    result = monitor.end_session()
"""

from __future__ import annotations

import os
import re
import select
import subprocess
import sys
from dataclasses import dataclass, field
from typing import Optional


# ============================================================================
# RTSP Session State
# ============================================================================

@dataclass
class RTSPSessionState:
    # Session
    session_active: bool = False
    session_id: str = ""
    session_id_set: bool = False
    session_id_mismatch: bool = False
    connection_closed: bool = False

    # Methods sent
    options_sent: bool = False
    describe_sent: bool = False
    describe_succeeded: bool = False
    setup_sent: bool = False
    setup_succeeded: bool = False
    play_sent: bool = False
    play_succeeded: bool = False
    pause_sent: bool = False
    record_sent: bool = False
    teardown_sent: bool = False

    # Transport
    transport_set: bool = False
    transport_proto: str = "tpNotSet"
    client_port: int = 0
    server_port: int = 0
    interleaved_mode: bool = False

    # Media state
    media_playing: bool = False
    media_recording: bool = False
    sdp_received: bool = False

    # CSeq
    cseq_sent: int = 0
    cseq_received: int = 0
    cseq_present: bool = False
    cseq_match: bool = False

    # Content
    content_length: int = 0

    # Sequence tracking
    sequence_number: int = 0
    last_method: str = "methNotSet"

    # RTSP state machine
    rtsp_state: str = "stateInit"


# ============================================================================
# RTSP Predicate Adapter
# ============================================================================

_RTSP_METHOD_MAP = {
    "OPTIONS":       "methOPTIONS",
    "DESCRIBE":      "methDESCRIBE",
    "ANNOUNCE":      "methANNOUNCE",
    "SETUP":         "methSETUP",
    "PLAY":          "methPLAY",
    "PAUSE":         "methPAUSE",
    "TEARDOWN":      "methTEARDOWN",
    "GET_PARAMETER": "methGET_PARAMETER",
    "SET_PARAMETER": "methSET_PARAMETER",
    "RECORD":        "methRECORD",
    "REDIRECT":      "methREDIRECT",
}


def _b(v: bool) -> str:
    return "true" if v else "false"


def _extract_method(buf: bytes) -> str:
    """Extract the RTSP method from a request line."""
    try:
        line = buf.split(b"\r\n")[0].decode("ascii", errors="replace")
    except Exception:
        return ""
    parts = line.split()
    if parts:
        return parts[0].upper()
    return ""


def _extract_response_code(buf: bytes) -> int:
    """Extract status code from 'RTSP/1.0 NNN Reason'."""
    try:
        line = buf.split(b"\r\n")[0].decode("ascii", errors="replace")
    except Exception:
        return 0
    parts = line.split()
    if len(parts) >= 2 and parts[0].startswith("RTSP/"):
        try:
            return int(parts[1])
        except ValueError:
            pass
    return 0


def _extract_header(buf: bytes, header_name: str) -> Optional[str]:
    """Extract value of a specific header (case-insensitive)."""
    header_lower = header_name.lower()
    try:
        text = buf.decode("ascii", errors="replace")
    except Exception:
        return None
    for line in text.split("\r\n"):
        if ":" in line:
            name, _, value = line.partition(":")
            if name.strip().lower() == header_lower:
                return value.strip()
    return None


def _extract_cseq(buf: bytes) -> Optional[int]:
    """Extract CSeq value from headers."""
    val = _extract_header(buf, "CSeq")
    if val is not None:
        try:
            return int(val.split()[0])
        except (ValueError, IndexError):
            pass
    return None


def _extract_session_id(buf: bytes) -> Optional[str]:
    """Extract Session header value (may include ;timeout=N)."""
    val = _extract_header(buf, "Session")
    if val is not None:
        return val.split(";")[0].strip()
    return None


def _extract_transport(buf: bytes) -> dict:
    """Parse Transport header into components."""
    val = _extract_header(buf, "Transport")
    if val is None:
        return {}
    result = {"raw": val}
    val_lower = val.lower()
    if "rtp/avp/tcp" in val_lower:
        result["proto"] = "tpRTP_AVP_TCP"
    elif "rtp/avp" in val_lower:
        result["proto"] = "tpRTP_AVP"
    elif "raw" in val_lower:
        result["proto"] = "tpRAW"
    else:
        result["proto"] = "tpNotSet"

    if "interleaved" in val_lower:
        result["interleaved"] = True

    # client_port=NNNN-NNNN
    m = re.search(r"client_port=(\d+)", val)
    if m:
        result["client_port"] = int(m.group(1))
    m = re.search(r"server_port=(\d+)", val)
    if m:
        result["server_port"] = int(m.group(1))
    return result


def _map_status_class(code: int) -> str:
    if code == 0:          return "scNotSet"
    if 100 <= code < 200:  return "scInformational"
    if 200 <= code < 300:  return "scSuccess"
    if 300 <= code < 400:  return "scRedirection"
    if 400 <= code < 500:  return "scClientError"
    if 500 <= code < 600:  return "scServerError"
    return "scNotSet"


def _is_request_malformed(buf: bytes) -> bool:
    """Basic malformation check for RTSP requests."""
    if len(buf) == 0:
        return True
    if len(buf) > 8192:
        return True
    if b"\x00" in buf:
        return True
    try:
        first_line = buf.split(b"\r\n")[0].decode("ascii", errors="replace")
    except Exception:
        return True
    parts = first_line.split()
    if len(parts) < 3:
        return True
    # Must have method, URI, RTSP/x.x
    if not parts[2].startswith("RTSP/"):
        return True
    return False


def _is_response_malformed(buf: bytes) -> bool:
    """Basic malformation check for RTSP responses."""
    if len(buf) == 0:
        return True
    try:
        first_line = buf.split(b"\r\n")[0].decode("ascii", errors="replace")
    except Exception:
        return True
    parts = first_line.split(None, 2)
    if len(parts) < 2:
        return True
    if not parts[0].startswith("RTSP/"):
        return True
    try:
        code = int(parts[1])
        if code < 100 or code > 599:
            return True
    except ValueError:
        return True
    return False


def _hex_trace(buf: bytes, max_bytes: int = 256) -> str:
    return buf[:max_bytes].hex()


class RTSPPredicateAdapter:
    """Maintains RTSP session state and builds predicate KV lines."""

    def __init__(self):
        self._state = RTSPSessionState()
        self._msg_id: int = 0

    def reset_session(self):
        self._state = RTSPSessionState()

    def _format_pred_line(
        self,
        method_enum: str,
        status_class: str,
        resp_code: int,
        cmd_malformed: bool,
        resp_malformed: bool,
        timeout: bool,
    ) -> str:
        s = self._state
        parts = [
            f"rtsp_method={method_enum}",
            f"rtsp_status_class={status_class}",
            f"resp_code={resp_code}",
            f"sequence_number={s.sequence_number}",
            f"cseq_sent={s.cseq_sent}",
            f"cseq_received={s.cseq_received}",
            f"client_port={s.client_port}",
            f"server_port={s.server_port}",
            f"content_length={s.content_length}",
            f"cmd_malformed={_b(cmd_malformed)}",
            f"resp_malformed={_b(resp_malformed)}",
            f"timeout={_b(timeout)}",
            f"connection_closed={_b(s.connection_closed)}",
            f"session_active={_b(s.session_active)}",
            f"session_id_set={_b(s.session_id_set)}",
            f"session_id_mismatch={_b(s.session_id_mismatch)}",
            f"describe_sent={_b(s.describe_sent)}",
            f"describe_succeeded={_b(s.describe_succeeded)}",
            f"setup_sent={_b(s.setup_sent)}",
            f"setup_succeeded={_b(s.setup_succeeded)}",
            f"play_sent={_b(s.play_sent)}",
            f"play_succeeded={_b(s.play_succeeded)}",
            f"pause_sent={_b(s.pause_sent)}",
            f"record_sent={_b(s.record_sent)}",
            f"teardown_sent={_b(s.teardown_sent)}",
            f"options_sent={_b(s.options_sent)}",
            f"transport_set={_b(s.transport_set)}",
            f"cseq_present={_b(s.cseq_present)}",
            f"cseq_match={_b(s.cseq_match)}",
            f"media_playing={_b(s.media_playing)}",
            f"media_recording={_b(s.media_recording)}",
            f"sdp_received={_b(s.sdp_received)}",
            f"interleaved_mode={_b(s.interleaved_mode)}",
            f"rtsp_state={s.rtsp_state}",
            f"transport_proto={s.transport_proto}",
        ]
        return " ".join(parts)

    def build_request_pred_line(self, buf: bytes) -> str:
        s = self._state
        method_str = _extract_method(buf)
        method_enum = _RTSP_METHOD_MAP.get(method_str, "methNotSet")
        s.last_method = method_enum

        # CSeq tracking
        cseq = _extract_cseq(buf)
        if cseq is not None:
            s.cseq_sent = cseq
            s.cseq_present = True
        else:
            s.cseq_present = False
        s.cseq_match = False  # reset, checked on response

        # Session ID tracking
        req_session = _extract_session_id(buf)
        if req_session is not None and s.session_id_set:
            s.session_id_mismatch = (req_session != s.session_id)
        else:
            s.session_id_mismatch = False

        # Method-specific state
        if method_enum == "methOPTIONS":
            s.options_sent = True
        elif method_enum == "methDESCRIBE":
            s.describe_sent = True
        elif method_enum == "methSETUP":
            s.setup_sent = True
            # Parse transport from request
            transport = _extract_transport(buf)
            if transport:
                if transport.get("client_port"):
                    s.client_port = transport["client_port"]
                if transport.get("interleaved"):
                    s.interleaved_mode = True
        elif method_enum == "methPLAY":
            s.play_sent = True
        elif method_enum == "methPAUSE":
            s.pause_sent = True
        elif method_enum == "methRECORD":
            s.record_sent = True
        elif method_enum == "methTEARDOWN":
            s.teardown_sent = True

        s.sequence_number += 1
        cmd_malformed = _is_request_malformed(buf)

        self._msg_id += 1
        line = self._format_pred_line(
            method_enum=method_enum, status_class="scNotSet",
            resp_code=0, cmd_malformed=cmd_malformed,
            resp_malformed=False, timeout=False,
        )
        line += f" msg_id={self._msg_id} dir=C2S trace={_hex_trace(buf)}"
        return line

    def build_response_pred_line(self, buf: bytes) -> str:
        s = self._state
        resp_code = _extract_response_code(buf)
        status_class = _map_status_class(resp_code)
        resp_malformed = _is_response_malformed(buf)

        # CSeq tracking
        resp_cseq = _extract_cseq(buf)
        if resp_cseq is not None:
            s.cseq_received = resp_cseq
            s.cseq_match = (resp_cseq == s.cseq_sent)
        else:
            s.cseq_match = False

        # Session ID from response
        resp_session = _extract_session_id(buf)
        if resp_session:
            if not s.session_id_set:
                s.session_id = resp_session
                s.session_id_set = True
                s.session_active = True

        # Content-Length
        cl = _extract_header(buf, "Content-Length")
        if cl:
            try:
                s.content_length = int(cl)
            except ValueError:
                pass

        # Content-Type check for SDP
        ct = _extract_header(buf, "Content-Type")
        if ct and "sdp" in ct.lower() and resp_code == 200:
            s.sdp_received = True

        is_success = (200 <= resp_code < 300)

        # Method-specific response handling
        if s.last_method == "methDESCRIBE" and is_success:
            s.describe_succeeded = True
        elif s.last_method == "methSETUP" and is_success:
            s.setup_succeeded = True
            s.transport_set = True
            s.rtsp_state = "stateReady"
            # Parse transport from response
            transport = _extract_transport(buf)
            if transport:
                if transport.get("proto"):
                    s.transport_proto = transport["proto"]
                if transport.get("server_port"):
                    s.server_port = transport["server_port"]
                if transport.get("interleaved"):
                    s.interleaved_mode = True
        elif s.last_method == "methPLAY" and is_success:
            s.play_succeeded = True
            s.media_playing = True
            s.media_recording = False
            s.rtsp_state = "statePlaying"
        elif s.last_method == "methPAUSE" and is_success:
            s.media_playing = False
            s.media_recording = False
            s.rtsp_state = "stateReady"
        elif s.last_method == "methRECORD" and is_success:
            s.media_recording = True
            s.media_playing = False
            s.rtsp_state = "stateRecording"
        elif s.last_method == "methTEARDOWN" and is_success:
            s.session_active = False
            s.media_playing = False
            s.media_recording = False
            s.transport_set = False
            s.rtsp_state = "stateInit"
        elif resp_code == 454:
            # Session Not Found
            s.session_active = False
        elif resp_code == 459:
            # Aggregate Operation Not Allowed
            pass

        s.sequence_number += 1

        self._msg_id += 1
        line = self._format_pred_line(
            method_enum=s.last_method, status_class=status_class,
            resp_code=resp_code, cmd_malformed=False,
            resp_malformed=resp_malformed, timeout=False,
        )
        line += f" msg_id={self._msg_id} dir=S2C trace={_hex_trace(buf)}"
        return line

    def build_timeout_pred_line(self) -> str:
        s = self._state
        s.sequence_number += 1
        self._msg_id += 1
        line = self._format_pred_line(
            method_enum=s.last_method, status_class="scNotSet",
            resp_code=0, cmd_malformed=False,
            resp_malformed=False, timeout=True,
        )
        line += f" msg_id={self._msg_id} dir=S2C trace="
        return line


# ============================================================================
# Monitor Bridge  (reused from ftp_monitor.py — same formula_parser protocol)
# ============================================================================

class MonitorBridge:
    """Launches formula_parser as a subprocess and communicates via pipes."""

    def __init__(self, eval_path: str, spec_path: str, protocol_tag: str = "rtsp"):
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


# ============================================================================
# Predicate Coverage Tracker (reused from ftp_monitor.py)
# ============================================================================

from spec_parser import (
    ParsedSpec, CoverageRule, RuleKind, parse_spec_file,
)


def _parse_kv_line(line: str) -> dict:
    kv = {}
    for token in line.split():
        eq = token.find('=')
        if eq > 0:
            kv[token[:eq]] = token[eq + 1:]
    return kv


_METADATA_KEYS = {'msg_id', 'dir', 'trace', 'sequence_number'}
_EXACT_INT_KEYS = {'resp_code'}


def _build_fingerprint_keys(spec: ParsedSpec) -> tuple:
    exact_keys = []
    bucket_keys = []
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
    for ename in spec.enums:
        exact_keys.append(ename)
    return exact_keys, bucket_keys


def _fingerprint(kv: dict, exact_keys: list, bucket_keys: list) -> tuple:
    vals = [kv.get(k, '') for k in exact_keys]
    for k in bucket_keys:
        try:
            vals.append('pos' if int(kv.get(k, '0')) > 0 else '0')
        except ValueError:
            vals.append('0')
    return tuple(vals)


def _eval_atoms(kv: dict, atoms) -> tuple:
    vec = []
    for atom in atoms:
        try:
            vec.append(1 if atom.evaluator(kv) else 0)
        except (ValueError, TypeError, KeyError):
            vec.append(0)
    return tuple(vec)


# Scoring weights
WEIGHT_NEW_FINGERPRINT = 1.0
WEIGHT_NEW_TRANSITION = 2.0
WEIGHT_NEW_RULE_VECTOR = 3.0
WEIGHT_PROXIMITY_STEP = 10.0
WEIGHT_ANTECEDENT_ACTIVE = 20.0
WEIGHT_CONSEQUENT_FALSIFIED = 50.0


@dataclass
class SessionResult:
    violated: bool = False
    coverage_score: float = 0.0
    is_interesting: bool = False
    new_fingerprints: int = 0
    new_transitions: int = 0
    new_rule_vectors: int = 0
    proximity_steps: int = 0


@dataclass
class CoverageStats:
    total_fingerprints: int = 0
    total_transitions: int = 0
    total_rule_vectors: int = 0
    max_proximity: dict = field(default_factory=dict)


class PredicateCoverageTracker:
    """Tracks coverage across three layers from predicate events."""

    def __init__(self, spec: ParsedSpec):
        self._rules = spec.rules
        self._fp_exact, self._fp_bucket = _build_fingerprint_keys(spec)
        self._seen_fps: set = set()
        self._seen_trans: set = set()
        self._seen_rule_vecs: set = set()
        self._max_proximity: list = [0] * len(self._rules)
        self._p_ever_active: list = [False] * len(self._rules)
        self._q_ever_false: list = [False] * len(self._rules)
        self._prev_fp_hash: Optional[int] = None
        self._trace_new_fps: int = 0
        self._trace_new_trans: int = 0
        self._trace_new_rvecs: int = 0
        self._trace_prox_steps: int = 0
        self._last_new_fps: int = 0
        self._last_new_trans: int = 0
        self._last_new_rvecs: int = 0
        self._last_prox_steps: int = 0

    def record_event(self, kv_line: str):
        kv = _parse_kv_line(kv_line)
        fp = _fingerprint(kv, self._fp_exact, self._fp_bucket)
        fp_hash = hash(fp)
        if fp_hash not in self._seen_fps:
            self._seen_fps.add(fp_hash)
            self._trace_new_fps += 1
        if self._prev_fp_hash is not None:
            trans = (self._prev_fp_hash, fp_hash)
            if trans not in self._seen_trans:
                self._seen_trans.add(trans)
                self._trace_new_trans += 1
        self._prev_fp_hash = fp_hash
        for rule_idx, rule in enumerate(self._rules):
            if rule.kind == RuleKind.IMPLICATION:
                self._record_implication(rule_idx, rule, kv)
            else:
                self._record_conjunction(rule_idx, rule, kv)

    def _record_conjunction(self, rule_idx, rule, kv):
        vec = _eval_atoms(kv, rule.atoms)
        key = (rule_idx, vec)
        if key not in self._seen_rule_vecs:
            self._seen_rule_vecs.add(key)
            self._trace_new_rvecs += 1
        n_sat = sum(vec)
        if n_sat > self._max_proximity[rule_idx]:
            self._trace_prox_steps += n_sat - self._max_proximity[rule_idx]
            self._max_proximity[rule_idx] = n_sat

    def _record_implication(self, rule_idx, rule, kv):
        p_vec = _eval_atoms(kv, rule.antecedent_atoms)
        p_key = (rule_idx, 'P', p_vec)
        if p_key not in self._seen_rule_vecs:
            self._seen_rule_vecs.add(p_key)
            self._trace_new_rvecs += 1
        p_sat = sum(p_vec)
        if p_sat > self._max_proximity[rule_idx]:
            self._trace_prox_steps += p_sat - self._max_proximity[rule_idx]
            self._max_proximity[rule_idx] = p_sat
        if not all(v == 1 for v in p_vec):
            return
        if not self._p_ever_active[rule_idx]:
            self._p_ever_active[rule_idx] = True
            self._trace_prox_steps += 1
        q_vec = _eval_atoms(kv, rule.consequent_atoms)
        q_key = (rule_idx, 'Q', q_vec)
        if q_key not in self._seen_rule_vecs:
            self._seen_rule_vecs.add(q_key)
            self._trace_new_rvecs += 1
        if all(v == 0 for v in q_vec) and not self._q_ever_false[rule_idx]:
            self._q_ever_false[rule_idx] = True
            self._trace_prox_steps += 1

    def end_trace(self) -> tuple:
        score = (
            self._trace_new_fps * WEIGHT_NEW_FINGERPRINT +
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
        proximity = {}
        for i, rule in enumerate(self._rules):
            proximity[i] = self._max_proximity[i]
        return CoverageStats(
            total_fingerprints=len(self._seen_fps),
            total_transitions=len(self._seen_trans),
            total_rule_vectors=len(self._seen_rule_vecs),
            max_proximity=proximity,
        )


# ============================================================================
# RTSPMonitor — Combined high-level API
# ============================================================================

class RTSPMonitor:
    """All-in-one: launches the monitor, translates raw RTSP buffers into
    predicate lines, feeds them to the evaluator, and tracks coverage."""

    def __init__(self, eval_path: str, spec_path: str):
        self.bridge = MonitorBridge(eval_path, spec_path, protocol_tag="rtsp")
        self.adapter = RTSPPredicateAdapter()
        self._spec = parse_spec_file(spec_path)
        self.coverage = PredicateCoverageTracker(self._spec)

    def start(self):
        self.bridge.start()

    def stop(self) -> int:
        return self.bridge.stop()

    @property
    def alive(self) -> bool:
        return self.bridge.alive

    def feed_request(self, buf: bytes):
        line = self.adapter.build_request_pred_line(buf)
        self.bridge.emit_line(line)
        self.coverage.record_event(line)

    def feed_response(self, buf: bytes):
        line = self.adapter.build_response_pred_line(buf)
        self.bridge.emit_line(line)
        self.coverage.record_event(line)

    def feed_timeout(self):
        line = self.adapter.build_timeout_pred_line()
        self.bridge.emit_line(line)
        self.coverage.record_event(line)

    def end_session(self) -> SessionResult:
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

    def check_violation(self) -> bool:
        self.bridge._drain_stdout(timeout_sec=0.0)
        return self.bridge.violation_detected

    def clear_violation(self):
        self.bridge.violation_detected = False
