#!/usr/bin/env bash
# migrate_grammarFuzzing.sh
#
# Replaces hardcoded paths in grammarFuzzing.ml with Config-relative
# paths for parallel instance support.
#
# Usage:
#   cp grammarFuzzing.ml grammarFuzzing.ml.bak
#   bash migrate_grammarFuzzing.sh grammarFuzzing.ml
#   diff grammarFuzzing.ml.bak grammarFuzzing.ml

set -euo pipefail

FILE="${1:?Usage: $0 <grammarFuzzing.ml>}"

echo "Migrating $FILE..."

# ================================================================
# 1. Config refs: add dereference operator
# ================================================================
sed -i \
  -e 's/Config\.num_packets/!Config.num_packets/g' \
  -e 's/Config\.num_queues/!Config.num_queues/g' \
  -e 's/Config\.start_symbol/!Config.start_symbol/g' \
  "$FILE"

# Fix any double-bangs from running twice
sed -i 's/!!Config/!Config/g' "$FILE"

# ================================================================
# 2. sync/ paths → (Config.sync_path "...")
#
# OCaml requires parens when a function application is an argument:
#   write_symbol_to_file (Config.sync_path "message.txt") merged_trace
# ================================================================

sed -i 's|"sync/message\.txt"|(Config.sync_path "message.txt")|g' "$FILE"
sed -i 's|"sync/response\.txt"|(Config.sync_path "response.txt")|g' "$FILE"
sed -i 's|"sync/oracle-response\.txt"|(Config.sync_path "oracle-response.txt")|g' "$FILE"
sed -i 's|"sync/score\.txt"|(Config.sync_path "score.txt")|g' "$FILE"
sed -i 's|"sync/placeholders-replace\.pkt"|(Config.sync_path "placeholders-replace.pkt")|g' "$FILE"

# Printf.sprintf "sync/message_%d.txt" i
# → (Config.sync_path (Printf.sprintf "message_%d.txt" i))
sed -i 's|Printf\.sprintf "sync/message_%d\.txt" \([^)]*\)|(Config.sync_path (Printf.sprintf "message_%d.txt" \1))|g' "$FILE"

# ================================================================
# 3. temporal-info/ paths → (Config.temporal_path "...")
# ================================================================

# Printf.sprintf "temporal-info/%d-queue-info.txt" x
# → (Config.temporal_path (Printf.sprintf "%d-queue-info.txt" x))
sed -i 's|Printf\.sprintf "temporal-info/%d-queue-info\.txt" \([^)]*\)|(Config.temporal_path (Printf.sprintf "%d-queue-info.txt" \1))|g' "$FILE"

sed -i 's|"temporal-info/NOTHING-queue-info\.txt"|(Config.temporal_path "NOTHING-queue-info.txt")|g' "$FILE"
sed -i 's|"temporal-info/CONFIRMED-queue-info\.txt"|(Config.temporal_path "CONFIRMED-queue-info.txt")|g' "$FILE"
sed -i 's|"temporal-info/ACCEPTED-queue-info\.txt"|(Config.temporal_path "ACCEPTED-queue-info.txt")|g' "$FILE"
sed -i 's|"temporal-info/OCaml-time-info\.csv"|(Config.temporal_path "OCaml-time-info.csv")|g' "$FILE"
sed -i 's|"temporal-info/queue-size-updates\.txt"|(Config.temporal_path "queue-size-updates.txt")|g' "$FILE"
sed -i 's|"temporal-info/sample-info\.txt"|(Config.temporal_path "sample-info.txt")|g' "$FILE"
sed -i 's|"temporal-info/iteration-times\.txt"|(Config.temporal_path "iteration-times.txt")|g' "$FILE"

# ================================================================
# 4. results/ paths → (Config.results_path "...")
# ================================================================

sed -i 's|"results/interesting_traces\.txt"|(Config.results_path "interesting_traces.txt")|g' "$FILE"

# ================================================================
# 5. grammar_hex_log.txt → !Config.grammar_hex_log
# ================================================================

sed -i 's|"grammar_hex_log\.txt"|!Config.grammar_hex_log|g' "$FILE"

# ================================================================
# Verify
# ================================================================
echo ""
echo "=== Remaining hardcoded paths (should be empty or only in comments): ==="
grep -n '"sync/\|"temporal-info/\|"results/\|"grammar_hex_log' "$FILE" | grep -v '(\*' || echo "(none — clean!)"

echo ""
echo "=== Counts: ==="
echo -n "  Config.sync_path:     "; grep -c 'Config\.sync_path' "$FILE" || echo "0"
echo -n "  Config.temporal_path: "; grep -c 'Config\.temporal_path' "$FILE" || echo "0"
echo -n "  Config.results_path:  "; grep -c 'Config\.results_path' "$FILE" || echo "0"
echo -n "  Config.grammar_hex:   "; grep -c 'Config\.grammar_hex_log' "$FILE" || echo "0"

echo ""
echo "Done. Review with: diff grammarFuzzing.ml.bak $FILE"
