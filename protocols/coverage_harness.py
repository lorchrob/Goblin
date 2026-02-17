#!/usr/bin/env python3
"""
coverage_harness.py — periodic LLVM coverage snapshot collector
================================================================

The ftp_driver.py manages the FTP server lifecycle (start/stop/restart per
trace), writing .profraw files to a shared directory.  This harness runs
alongside and periodically:

  1. Merges ALL accumulated .profraw files  (cumulative)
  2. Generates llvm-cov report + JSON export
  3. Saves snapshot to hour_NN/ folder
  4. Repeats for --hours iterations

Three processes, three terminals:

  # Terminal 1 — Driver (manages FTP + writes profraw)
  python3 ftp_driver.py \
    --server-binary ./fftp --server-config ./fftp.conf \
    --profraw-dir coverage_results/profraw \
    --restart-every 1 \
    --monitor ./formula_parser --spec ftp.txt

  # Terminal 2 — Coverage collector (this script)
  python3 coverage_harness.py \
    --binary ./fftp \
    --profraw-dir coverage_results/profraw \
    --output-dir coverage_results \
    --hours 24 --interval 3600

  # Terminal 3 — OCaml fuzzer
  ./your_fuzzer_binary

Output:
  coverage_results/
    profraw/                   (written by driver, read by this script)
    hour_01/
      merged.profdata
      summary.txt
      coverage.json
      meta.json
    ...
    hour_24/
    campaign_summary.json
"""

import argparse
import json
import logging
import os
import shutil
import signal
import subprocess
import sys
import time
from datetime import datetime, timedelta
from pathlib import Path

logger = logging.getLogger("coverage_harness")


# ============================================================================
# Coverage collection
# ============================================================================

class CoverageCollector:
    """Merges .profraw files and generates coverage reports."""

    def __init__(self, binary: str, source_dir: str = "",
                 llvm_profdata: str = "llvm-profdata",
                 llvm_cov: str = "llvm-cov"):
        self.binary = str(Path(binary).resolve())
        self.source_dir = source_dir
        self.llvm_profdata = llvm_profdata
        self.llvm_cov = llvm_cov

    def collect(self, profraw_dir: str, output_dir: str,
                hour: int, meta: dict = None) -> bool:
        """Merge all profraw files in profraw_dir and generate reports."""
        out = Path(output_dir)
        out.mkdir(parents=True, exist_ok=True)

        profraw_files = sorted(Path(profraw_dir).glob("*.profraw"))
        if not profraw_files:
            logger.warning("No .profraw files in %s for hour %d", profraw_dir, hour)
            self._write_meta(out, hour, meta, success=False)
            return False

        profdata = out / "merged.profdata"

        # --- Merge ---
        logger.info("Merging %d .profraw files -> %s", len(profraw_files), profdata)
        merge_cmd = [
            self.llvm_profdata, "merge", "-sparse",
            "-o", str(profdata),
        ] + [str(f) for f in profraw_files]

        try:
            result = subprocess.run(merge_cmd, capture_output=True, text=True, timeout=120)
            if result.returncode != 0:
                logger.error("llvm-profdata merge failed: %s", result.stderr[:500])
                self._write_meta(out, hour, meta, success=False)
                return False
        except (subprocess.TimeoutExpired, OSError) as e:
            logger.error("llvm-profdata error: %s", e)
            self._write_meta(out, hour, meta, success=False)
            return False

        # --- Text summary ---
        summary_file = out / "summary.txt"
        report_cmd = [self.llvm_cov, "report", self.binary,
                      f"-instr-profile={profdata}"]
        try:
            result = subprocess.run(report_cmd, capture_output=True, text=True, timeout=60)
            summary_file.write_text(result.stdout + result.stderr)
        except (subprocess.TimeoutExpired, OSError) as e:
            summary_file.write_text(f"ERROR: {e}\n")

        # --- JSON export ---
        json_file = out / "coverage.json"
        export_cmd = [self.llvm_cov, "export", self.binary,
                      f"-instr-profile={profdata}", "--format=text"]
        try:
            result = subprocess.run(export_cmd, capture_output=True, text=True, timeout=60)
            json_file.write_text(result.stdout)
        except (subprocess.TimeoutExpired, OSError) as e:
            json_file.write_text(json.dumps({"error": str(e)}))

        # --- Parse and write meta ---
        cov_summary = self._parse_coverage_json(json_file)
        self._write_meta(out, hour, meta, success=True,
                         cov_summary=cov_summary, num_profraw=len(profraw_files))

        logger.info("Hour %d coverage: %s", hour, self._format_summary(cov_summary))
        return True

    def _parse_coverage_json(self, json_path: Path) -> dict:
        try:
            data = json.loads(json_path.read_text())
            totals = data.get("data", [{}])[0].get("totals", {})
            return {
                "lines_covered": totals.get("lines", {}).get("covered", 0),
                "lines_total": totals.get("lines", {}).get("count", 0),
                "functions_covered": totals.get("functions", {}).get("covered", 0),
                "functions_total": totals.get("functions", {}).get("count", 0),
                "regions_covered": totals.get("regions", {}).get("covered", 0),
                "regions_total": totals.get("regions", {}).get("count", 0),
                "branches_covered": totals.get("branches", {}).get("covered", 0),
                "branches_total": totals.get("branches", {}).get("count", 0),
            }
        except Exception as e:
            logger.warning("Could not parse coverage JSON: %s", e)
            return {}

    def _format_summary(self, s: dict) -> str:
        if not s:
            return "(no data)"
        parts = []
        for kind in ["lines", "functions", "regions", "branches"]:
            covered = s.get(f"{kind}_covered", 0)
            total = s.get(f"{kind}_total", 0)
            if total > 0:
                pct = 100.0 * covered / total
                parts.append(f"{kind}={covered}/{total} ({pct:.1f}%)")
        return ", ".join(parts) if parts else "(no data)"

    def _write_meta(self, out: Path, hour: int, meta: dict,
                    success: bool, cov_summary: dict = None, num_profraw: int = 0):
        meta_data = {
            "hour": hour,
            "timestamp": datetime.now().isoformat(),
            "success": success,
            "num_profraw_files": num_profraw,
        }
        if meta:
            meta_data.update(meta)
        if cov_summary:
            meta_data["coverage"] = cov_summary
        (out / "meta.json").write_text(json.dumps(meta_data, indent=2) + "\n")


# ============================================================================
# Campaign: periodic collection loop
# ============================================================================

class CoverageCampaign:
    """Runs periodic coverage snapshots over a given number of hours."""

    def __init__(self, binary: str, profraw_dir: str, output_dir: str,
                 source_dir: str = "", hours: int = 24,
                 interval_sec: float = 3600.0,
                 llvm_profdata: str = "llvm-profdata",
                 llvm_cov: str = "llvm-cov"):
        self.profraw_dir = str(Path(profraw_dir).resolve())
        self.output_dir = Path(output_dir)
        self.hours = hours
        self.interval_sec = interval_sec
        self._running = True

        self.collector = CoverageCollector(
            binary=binary, source_dir=source_dir,
            llvm_profdata=llvm_profdata, llvm_cov=llvm_cov,
        )

    def run(self):
        def shutdown(signum, frame):
            logger.info("Received signal %d — stopping", signum)
            self._running = False
        signal.signal(signal.SIGINT, shutdown)
        signal.signal(signal.SIGTERM, shutdown)

        self.output_dir.mkdir(parents=True, exist_ok=True)
        os.makedirs(self.profraw_dir, exist_ok=True)
        campaign_start = datetime.now()

        logger.info("=" * 60)
        logger.info("Coverage collector: %d snapshots, %ds apart",
                     self.hours, int(self.interval_sec))
        logger.info("Watching profraw dir: %s", self.profraw_dir)
        logger.info("Output: %s", self.output_dir)
        logger.info("=" * 60)

        for hour in range(1, self.hours + 1):
            if not self._running:
                break

            logger.info("Snapshot %d/%d — sleeping until %s...",
                        hour, self.hours,
                        (datetime.now() + timedelta(
                            seconds=self.interval_sec)).strftime("%H:%M:%S"))

            # Sleep (interruptible)
            deadline = time.monotonic() + self.interval_sec
            while time.monotonic() < deadline and self._running:
                time.sleep(min(10.0, deadline - time.monotonic()))

            if not self._running:
                break

            # Collect
            hour_dir = str(self.output_dir / f"hour_{hour:02d}")
            elapsed = (datetime.now() - campaign_start).total_seconds()
            meta = {"elapsed_seconds": round(elapsed, 1)}
            self.collector.collect(self.profraw_dir, hour_dir, hour, meta)

        # Campaign summary
        elapsed_total = (datetime.now() - campaign_start).total_seconds()
        summary = {
            "campaign_start": campaign_start.isoformat(),
            "campaign_end": datetime.now().isoformat(),
            "elapsed_seconds": round(elapsed_total, 1),
            "hours_planned": self.hours,
            "interval_seconds": self.interval_sec,
        }
        for h in range(self.hours, 0, -1):
            meta_path = self.output_dir / f"hour_{h:02d}" / "meta.json"
            if meta_path.exists():
                try:
                    summary["final_coverage"] = json.loads(
                        meta_path.read_text()).get("coverage", {})
                    break
                except Exception:
                    pass

        (self.output_dir / "campaign_summary.json").write_text(
            json.dumps(summary, indent=2) + "\n")
        logger.info("Campaign complete: %.1f hours", elapsed_total / 3600)


# ============================================================================
# CLI
# ============================================================================

def main():
    parser = argparse.ArgumentParser(
        description="Periodic LLVM coverage snapshot collector for FTP fuzzing",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
The driver (ftp_driver.py --server-binary) manages the FTP process and writes
.profraw files.  This script periodically merges them into coverage reports.

Examples:
  # Full 24-hour campaign
  python3 coverage_harness.py \\
    --binary ./fftp \\
    --profraw-dir coverage_results/profraw \\
    --output-dir coverage_results

  # Quick test: 2 snapshots, 5 minutes apart
  python3 coverage_harness.py \\
    --binary ./fftp \\
    --profraw-dir coverage_results/profraw \\
    --hours 2 --interval 300
""",
    )

    parser.add_argument("--binary", required=True,
                        help="Path to fftp binary (for llvm-cov, not executed)")
    parser.add_argument("--profraw-dir", required=True,
                        help="Directory where driver writes .profraw files")
    parser.add_argument("--source-dir", default="",
                        help="Path to LightFTP source (for source-level reports)")
    parser.add_argument("--output-dir", default="coverage_results",
                        help="Directory for coverage snapshots (default: coverage_results)")
    parser.add_argument("--hours", type=int, default=24,
                        help="Number of snapshots (default: 24)")
    parser.add_argument("--interval", type=float, default=3600.0,
                        help="Seconds between snapshots (default: 3600)")
    parser.add_argument("--llvm-profdata", default="llvm-profdata")
    parser.add_argument("--llvm-cov", default="llvm-cov")
    parser.add_argument("--log-level", default="INFO",
                        choices=["DEBUG", "INFO", "WARNING", "ERROR"])

    args = parser.parse_args()

    logging.basicConfig(
        level=getattr(logging, args.log_level),
        format="%(asctime)s [%(name)s] %(levelname)s: %(message)s",
        datefmt="%Y-%m-%d %H:%M:%S",
    )

    if not Path(args.binary).exists():
        logger.error("Binary not found: %s", args.binary)
        sys.exit(1)

    for name, path in [("llvm-profdata", args.llvm_profdata),
                       ("llvm-cov", args.llvm_cov)]:
        if not shutil.which(path):
            logger.error("%s not found: %s", name, path)
            sys.exit(1)

    campaign = CoverageCampaign(
        binary=args.binary,
        profraw_dir=args.profraw_dir,
        output_dir=args.output_dir,
        source_dir=args.source_dir,
        hours=args.hours,
        interval_sec=args.interval,
        llvm_profdata=args.llvm_profdata,
        llvm_cov=args.llvm_cov,
    )
    campaign.run()


if __name__ == "__main__":
    main()
