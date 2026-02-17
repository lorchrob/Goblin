#!/usr/bin/env bash
# launch_campaign.sh — Run multiple protocol fuzzing jobs from a config file
#
# Usage:
#   bash launch_campaign.sh campaign.json
#   bash launch_campaign.sh campaign.json --dry-run
#
# campaign.json example:
#   {
#     "jobs": [
#       {
#         "protocol": "ftp",
#         "mode": "3",
#         "instances": 2,
#         "server": "./fftp",
#         "server_dir": "",
#         "monitor": "./formula_parser",
#         "spec": "protocols/FTP/ftp.txt",
#         "extra_driver_args": "--server-config ./fftp.conf"
#       },
#       {
#         "protocol": "rtsp",
#         "mode": "3",
#         "instances": 3,
#         "server": "./live555MediaServer",
#         "server_dir": "./media",
#         "monitor": "./formula_parser",
#         "spec": "protocols/RTSP/rtsp.txt",
#         "extra_driver_args": "--stream-path /test.264"
#       }
#     ],
#     "goblin": "./goblin",
#     "hours": 24,
#     "interval": 3600
#   }

set -euo pipefail

CONFIG="${1:?Usage: $0 <campaign.json> [--dry-run]}"
DRY_RUN=false
[ "${2:-}" = "--dry-run" ] && DRY_RUN=true

if ! command -v jq &>/dev/null; then
  echo "ERROR: jq is required. Install with: apt install jq"
  exit 1
fi

if [ ! -f "$CONFIG" ]; then
  echo "ERROR: Config file not found: $CONFIG"
  exit 1
fi

# ================================================================
# Read global config
# ================================================================
GOBLIN=$(jq -r '.goblin // "./goblin"' "$CONFIG")
HOURS=$(jq -r '.hours // 24' "$CONFIG")
INTERVAL=$(jq -r '.interval // 3600' "$CONFIG")
NUM_JOBS=$(jq '.jobs | length' "$CONFIG")

echo "============================================"
echo "Campaign Launcher"
echo "  Config:    $CONFIG"
echo "  Jobs:      $NUM_JOBS"
echo "  Goblin:    $GOBLIN"
echo "  Hours:     $HOURS"
echo "  Interval:  $INTERVAL"
echo "============================================"

# ================================================================
# Protocol → base port mapping
# ================================================================
base_port_for() {
  case "$1" in
    ftp)  echo 2121;;
    rtsp) echo 8554;;
    wpa)  echo 0;;
    *)    echo 9000;;
  esac
}

driver_for() {
  case "$1" in
    ftp)  echo "ftp_driver.py";;
    rtsp) echo "rtsp_driver.py";;
    wpa)  echo "wpa_driver.py";;
    *)    echo "${1}_driver.py";;
  esac
}

# ================================================================
# PID tracking
# ================================================================
ALL_PIDS=()

cleanup() {
  echo ""
  echo "Shutting down all campaign processes (${#ALL_PIDS[@]} PIDs)..."
  for pid in "${ALL_PIDS[@]}"; do
    kill "$pid" 2>/dev/null || true
  done
  # Give processes time to flush profraw
  sleep 2
  for pid in "${ALL_PIDS[@]}"; do
    kill -9 "$pid" 2>/dev/null || true
  done
  wait 2>/dev/null
  echo "Campaign stopped."
}
trap cleanup EXIT INT TERM

# ================================================================
# Per-protocol instance counter (avoids port collision
# when multiple jobs use the same protocol)
# ================================================================
declare -A PROTO_OFFSET

# ================================================================
# Launch each job
# ================================================================
for j in $(seq 0 $((NUM_JOBS - 1))); do
  PROTOCOL=$(jq -r ".jobs[$j].protocol" "$CONFIG")
  MODE=$(jq -r ".jobs[$j].mode // \"3\"" "$CONFIG")
  INSTANCES=$(jq -r ".jobs[$j].instances // 1" "$CONFIG")
  SERVER=$(jq -r ".jobs[$j].server // \"\"" "$CONFIG")
  SERVER_DIR=$(jq -r ".jobs[$j].server_dir // \"\"" "$CONFIG")
  MONITOR=$(jq -r ".jobs[$j].monitor // \"\"" "$CONFIG")
  SPEC=$(jq -r ".jobs[$j].spec // \"\"" "$CONFIG")
  EXTRA=$(jq -r ".jobs[$j].extra_driver_args // \"\"" "$CONFIG")

  BASE_PORT=$(base_port_for "$PROTOCOL")
  DRIVER=$(driver_for "$PROTOCOL")
  OFFSET=${PROTO_OFFSET[$PROTOCOL]:-0}

  echo ""
  echo "=== Job $j: $PROTOCOL (mode $MODE, $INSTANCES instances, port offset $OFFSET) ==="

  for i in $(seq 0 $((INSTANCES - 1))); do
    PORT=$((BASE_PORT + OFFSET + i))
    RUN_DIR="runs/${PROTOCOL}/mode${MODE}/instance_${i}"

    mkdir -p "${RUN_DIR}/sync" \
             "${RUN_DIR}/temporal-info" \
             "${RUN_DIR}/results" \
             "${RUN_DIR}/profraw-dir"

    echo "  instance $i: port=$PORT dir=$RUN_DIR"

    if $DRY_RUN; then
      echo "    [dry-run] would start driver, harness, fuzzer"
      continue
    fi

    # ---- Driver ----
    DRIVER_CMD=(
      python3 "$DRIVER"
      --host 127.0.0.1 --port "$PORT"
      --sync-dir "${RUN_DIR}/sync"
      --profraw-dir "${RUN_DIR}/profraw-dir"
      --restart-every 1
    )
    [ -n "$SERVER" ]  && DRIVER_CMD+=(--server-binary "$SERVER")
    [ -n "$SERVER_DIR" ] && DRIVER_CMD+=(--server-dir "$SERVER_DIR")
    [ -n "$MONITOR" ] && [ -n "$SPEC" ] && DRIVER_CMD+=(--monitor "$MONITOR" --spec "$SPEC")
    # shellcheck disable=SC2086
    [ -n "$EXTRA" ] && DRIVER_CMD+=($EXTRA)

    "${DRIVER_CMD[@]}" > "${RUN_DIR}/driver.log" 2>&1 &
    ALL_PIDS+=($!)

    # ---- Coverage harness ----
    if [ -n "$SERVER" ]; then
      python3 coverage_harness.py \
        --binary "$SERVER" \
        --profraw-dir "${RUN_DIR}/profraw-dir" \
        --output-dir "${RUN_DIR}/coverage" \
        --hours "$HOURS" --interval "$INTERVAL" \
        > "${RUN_DIR}/harness.log" 2>&1 &
      ALL_PIDS+=($!)
    fi

    # ---- OCaml fuzzer ----
    $GOBLIN --saecred \
      --protocol "$PROTOCOL" \
      --mode "$MODE" \
      --instance "$i" \
      > "${RUN_DIR}/fuzzer.log" 2>&1 &
    ALL_PIDS+=($!)
  done

  # Update offset so next job of same protocol gets different ports
  PROTO_OFFSET[$PROTOCOL]=$((OFFSET + INSTANCES))
done

if $DRY_RUN; then
  echo ""
  echo "Dry run complete. No processes started."
  exit 0
fi

echo ""
echo "============================================"
echo "Campaign running: ${#ALL_PIDS[@]} processes"
echo "  PIDs: ${ALL_PIDS[*]}"
echo ""
echo "  Logs:  runs/<protocol>/mode<M>/instance_<N>/*.log"
echo "  Traces: runs/<protocol>/mode<M>/instance_<N>/results/"
echo ""
echo "Press Ctrl-C to stop all."
echo "============================================"

wait
