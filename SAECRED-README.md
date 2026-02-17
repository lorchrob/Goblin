# Running the Grammar-Based Protocol Fuzzer

## Prerequisites

- OCaml toolchain with `dune` (for building Goblin)
- Python 3.10+
- Clang/LLVM toolchain (`clang`, `llvm-profdata`, `llvm-cov`) for coverage instrumentation
- `jq` (for campaign launcher)
- Target server source code (LightFTP, live555, etc.)

## Building

### 1. Build Goblin (OCaml fuzzer)

```bash
make
```

This produces the `./goblin` binary.

### 2. Build an instrumented target server

Coverage instrumentation requires Clang with profiling flags.

**LightFTP (FTP target):**

```bash
cd lightftp/src
export CC=clang
export CFLAGS="-fprofile-instr-generate -fcoverage-mapping -g"
export LDFLAGS="-fprofile-instr-generate"
make clean && make
```

The binary at `lightftp/src/Release/fftp` will emit `.profraw` files on exit.

**live555 (RTSP target):**

```bash
cd live555
export COMPILE_OPTS="-fprofile-instr-generate -fcoverage-mapping -g"
./genMakefiles linux
# Edit config.linux to add COMPILE_OPTS to C_FLAGS and LINK_OPTS
make clean && make
```

The binary at `live555/mediaServer/live555MediaServer` will emit `.profraw` files on exit.

**Important:** The target server needs a signal handler that calls `exit(0)` on SIGTERM so the LLVM coverage runtime flushes profraw data. See `ftp_driver.py` / `rtsp_driver.py` forkserver documentation for details.

### 3. Prepare media files (RTSP only)

live555 serves files from its working directory. Place a test H.264 file:

```bash
mkdir -p live555/mediaServer/media
cp test.264 live555/mediaServer/media/
```

## Directory Structure

Grammar and spec files must be in the following layout:

```
protocols/
  FTP/
    grammar/
      ftp_auth.txt
      ftp_datachannel.txt
      ftp_path.txt
      ftp_site.txt
    ftp.txt              # LTL spec
  RTSP/
    grammar/
      rtsp_session.txt
      rtsp_setup.txt
      rtsp_media.txt
      rtsp_announce.txt
      rtsp_adversarial.txt
    rtsp.txt             # LTL spec
```

## Running a Single Instance

A single fuzzing run requires three processes in three terminals.

### Terminal 1: Driver (manages target server + monitors predicates)

**FTP:**

```bash
python3 ftp_driver.py \
  --host 127.0.0.1 \
  --port 2121 \
  --server-binary ./lightftp/src/Release/fftp \
  --server-config ./lightftp/src/Release/fftp.conf \
  --profraw-dir runs/ftp/mode3/instance_0/profraw-dir \
  --sync-dir runs/ftp/mode3/instance_0/sync \
  --monitor ./formula_parser \
  --spec protocols/FTP/ftp.txt \
  --restart-every 1
```

**RTSP:**

```bash
python3 rtsp_driver.py \
  --host 127.0.0.1 \
  --port 8554 \
  --server-binary ./live555/mediaServer/live555MediaServer \
  --server-dir ./live555/mediaServer/media \
  --stream-path /test.264 \
  --profraw-dir runs/rtsp/mode3/instance_0/profraw-dir \
  --sync-dir runs/rtsp/mode3/instance_0/sync \
  --monitor ./formula_parser \
  --spec protocols/RTSP/rtsp.txt \
  --restart-every 1
```

Add `--forkserver` to either command for ~20x faster execution (requires forkserver support compiled into the target).

### Terminal 2: Coverage harness (periodic profraw merging)

```bash
python3 coverage_harness.py \
  --binary ./lightftp/src/Release/fftp \
  --profraw-dir runs/ftp/mode3/instance_0/profraw-dir \
  --output-dir runs/ftp/mode3/instance_0/coverage \
  --hours 24 \
  --interval 3600
```

Replace `--binary` with the RTSP server binary for RTSP runs. The harness is protocol-agnostic.

### Terminal 3: OCaml fuzzer (grammar mutation engine)

```bash
./goblin --saecred --protocol ftp --mode 3 --instance 0
```

**Flags:**

| Flag | Values | Description |
|------|--------|-------------|
| `--protocol` | `ftp`, `rtsp`, `wpa` | Selects grammars, state queues, and packet types |
| `--mode` | `1`, `2`, `3` | Fuzzing mode (Mode3 = interesting inputs to single queue) |
| `--instance` | `0`, `1`, `2`, ... | Parallel instance ID (determines sync directory and port) |

## Running Multiple Instances in Parallel

### Option A: Launch script (single protocol)

```bash
bash launch_parallel.sh \
  --protocol rtsp \
  --mode 3 \
  --instances 4 \
  --server ./live555/mediaServer/live555MediaServer \
  --server-dir ./live555/mediaServer/media \
  --monitor ./formula_parser \
  --spec protocols/RTSP/rtsp.txt \
  --stream-path /test.264
```

This starts 4 isolated fuzzer triplets (driver + harness + fuzzer) on ports 8554–8557.

### Option B: Campaign file (multiple protocols)

Create a `campaign.json`:

```json
{
  "goblin": "./goblin",
  "hours": 24,
  "interval": 3600,
  "jobs": [
    {
      "protocol": "ftp",
      "mode": "3",
      "instances": 2,
      "server": "./lightftp/src/Release/fftp",
      "monitor": "./formula_parser",
      "spec": "protocols/FTP/ftp.txt",
      "extra_driver_args": "--server-config ./lightftp/src/Release/fftp.conf"
    },
    {
      "protocol": "rtsp",
      "mode": "3",
      "instances": 3,
      "server": "./live555/mediaServer/live555MediaServer",
      "server_dir": "./live555/mediaServer/media",
      "monitor": "./formula_parser",
      "spec": "protocols/RTSP/rtsp.txt",
      "extra_driver_args": "--stream-path /test.264"
    }
  ]
}
```

Run it:

```bash
bash launch_campaign.sh campaign.json
```

Dry run (shows what would launch without starting anything):

```bash
bash launch_campaign.sh campaign.json --dry-run
```

Press Ctrl-C to stop all instances.

## Instance Isolation

Each instance gets a fully isolated directory tree:

```
runs/
  ftp/
    mode3/
      instance_0/
        sync/             # OCaml ↔ Driver IPC (message.txt, response.txt, etc.)
        temporal-info/    # Timing diagnostics
        results/          # Interesting traces
        profraw-dir/      # LLVM coverage profraw files
        coverage/         # Merged coverage snapshots (hour_01/, hour_02/, ...)
        driver.log
        harness.log
        fuzzer.log
      instance_1/
        ...
  rtsp/
    mode3/
      instance_0/
        ...
```

Ports are auto-assigned to avoid collisions:

| Protocol | Base Port | Instance 0 | Instance 1 | Instance 2 |
|----------|-----------|------------|------------|------------|
| FTP      | 2121      | 2121       | 2122       | 2123       |
| RTSP     | 8554      | 8554       | 8555       | 8556       |

If the same protocol appears in multiple jobs (e.g., FTP mode 2 and FTP mode 3), ports are offset automatically so they don't collide.

## Monitoring a Run

### Logs

```bash
# Watch a specific instance
tail -f runs/rtsp/mode3/instance_0/driver.log
tail -f runs/rtsp/mode3/instance_0/fuzzer.log

# Watch all instances
tail -f runs/*/*/instance_*/driver.log
```

### Coverage snapshots

After each hourly snapshot:

```bash
cat runs/ftp/mode3/instance_0/coverage/hour_01/meta.json
cat runs/ftp/mode3/instance_0/coverage/hour_01/summary.txt
```

### Interesting traces

```bash
cat runs/ftp/mode3/instance_0/results/interesting_traces.txt
```

### Queue diagnostics

```bash
cat runs/rtsp/mode3/instance_0/temporal-info/queue-size-updates.txt
cat runs/rtsp/mode3/instance_0/temporal-info/OCaml-time-info.csv
```

## Quick Smoke Test

To verify the pipeline works without a long campaign:

```bash
# 1. Build
make

# 2. Create directories
mkdir -p runs/ftp/mode3/instance_0/{sync,temporal-info,results,profraw-dir}

# 3. Start driver
python3 ftp_driver.py \
  --port 2121 \
  --server-binary ./fftp \
  --sync-dir runs/ftp/mode3/instance_0/sync \
  --profraw-dir runs/ftp/mode3/instance_0/profraw-dir &

# 4. Start fuzzer (will begin generating and sending traces)
./goblin --saecred --protocol ftp --mode 3 --instance 0

# Watch IPC happening:
watch -n 0.5 cat runs/ftp/mode3/instance_0/sync/message.txt
```

## Troubleshooting

**100% timeout rate:** Check that the grammar CRLF terminals use `BitVector { <CRLF> = 0x0A0D; }` and not literal `\r\n` strings. Literal escape sequences get hex-encoded as `5c725c6e` instead of real CRLF `0d0a`.

**No profraw files:** The target server must call `exit(0)` on SIGTERM (not `_exit()` or raw termination). LLVM registers an `atexit` hook to flush coverage data. Add a signal handler: `signal(SIGTERM, handler)` where handler calls `exit(0)`.

**Port already in use:** Another instance or leftover server process is bound to that port. Kill it with `lsof -i :2121` or change the instance number.

**Driver hangs on startup:** The server may need time to bind its socket. The driver waits 1 second by default. Increase with `--startup-delay 3`.

**Formula parser not found:** The `--monitor` flag expects the `formula_parser` binary (built from the LTL spec compiler). Build it separately and ensure it's on PATH or specify the full path.
