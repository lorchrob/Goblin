#!/usr/bin/env bash
# ============================================================================
# build_lightftp_coverage.sh
#
# Compile LightFTP with:
#   1. AFL instrumentation (afl-clang-fast) for fuzzing feedback
#   2. LLVM source-based coverage (-fprofile-instr-generate -fcoverage-mapping)
#      so we can collect code coverage snapshots at runtime
#
# Usage:
#   ./build_lightftp_coverage.sh /path/to/lightftp/src
#
# The built binary will be at: <src>/Release/fftp
# ============================================================================

set -euo pipefail

LIGHTFTP_SRC="${1:?Usage: $0 /home/pirwani/lightftp/src/}"
LIGHTFTP_SRC="$(realpath "$LIGHTFTP_SRC")"
RELEASE_DIR="$LIGHTFTP_SRC/Release"

if [ ! -f "$RELEASE_DIR/makefile" ]; then
    echo "ERROR: Cannot find $RELEASE_DIR/makefile"
    echo "  Expected LightFTP source tree at: $LIGHTFTP_SRC"
    exit 1
fi

# ---------------------------------------------------------------------------
# Locate tools
# ---------------------------------------------------------------------------

AFL_CLANG_FAST="${AFL_CLANG_FAST:-afl-clang-fast}"
LLVM_PROFDATA="${LLVM_PROFDATA:-llvm-profdata}"
LLVM_COV="${LLVM_COV:-llvm-cov}"

for tool in "$AFL_CLANG_FAST" "$LLVM_PROFDATA" "$LLVM_COV"; do
    if ! command -v "$tool" &>/dev/null; then
        echo "ERROR: $tool not found in PATH"
        echo "  Install AFL++ and LLVM, or set AFL_CLANG_FAST / LLVM_PROFDATA / LLVM_COV"
        exit 1
    fi
done

echo "=== Tools ==="
echo "  Compiler:    $AFL_CLANG_FAST  ($(command -v "$AFL_CLANG_FAST"))"
echo "  Profdata:    $LLVM_PROFDATA   ($(command -v "$LLVM_PROFDATA"))"
echo "  Coverage:    $LLVM_COV        ($(command -v "$LLVM_COV"))"

# ---------------------------------------------------------------------------
# Coverage flags
# ---------------------------------------------------------------------------
# -fprofile-instr-generate  — emit __llvm_profile_* instrumentation
# -fcoverage-mapping        — embed source-mapping metadata in the binary
# These are orthogonal to AFL's instrumentation (which uses its own pass).

COV_CFLAGS="-fprofile-instr-generate -fcoverage-mapping"
COV_LDFLAGS="-fprofile-instr-generate"

# ---------------------------------------------------------------------------
# Build
# ---------------------------------------------------------------------------

echo ""
echo "=== Building LightFTP with coverage instrumentation ==="
echo "  Source:  $LIGHTFTP_SRC"
echo "  CFLAGS:  $COV_CFLAGS"
echo "  LDFLAGS: $COV_LDFLAGS"
echo ""

cd "$RELEASE_DIR"

# Clean previous build
make clean 2>/dev/null || true

# Build with afl-clang-fast + coverage flags
# LightFTP's Makefile respects CC, CFLAGS, LDFLAGS
make fftp \
    CC="$AFL_CLANG_FAST" \
    CFLAGS="-O2 -Wall $COV_CFLAGS" \
    LDFLAGS="$COV_LDFLAGS"

BINARY="$RELEASE_DIR/fftp"
if [ ! -f "$BINARY" ]; then
    echo "ERROR: Build succeeded but $BINARY not found"
    exit 1
fi

echo ""
echo "=== Build successful ==="
echo "  Binary:  $BINARY"
echo ""
echo "=== Runtime usage ==="
echo "  Set LLVM_PROFILE_FILE to control where .profraw files go:"
echo ""
echo "    export LLVM_PROFILE_FILE=/path/to/profraw/fftp-%p-%m.profraw"
echo "    $BINARY /path/to/fftp.conf"
echo ""
echo "  On process exit (SIGTERM), the profraw is flushed."
echo "  Then merge + report:"
echo ""
echo "    $LLVM_PROFDATA merge -o merged.profdata /path/to/profraw/*.profraw"
echo "    $LLVM_COV report $BINARY -instr-profile=merged.profdata"
echo "    $LLVM_COV export $BINARY -instr-profile=merged.profdata --format=text > coverage.json"
echo ""
echo "  Or use coverage_harness.py for automated hourly collection."
