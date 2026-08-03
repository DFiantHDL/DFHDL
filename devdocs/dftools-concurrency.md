# DFTools Concurrency & Test Flakiness — Investigation

> Measured root causes of the intermittent failures seen when tests run external EDA tools
> **concurrently** (full `sbtn test` runs), on **Windows 10 + WSL2 + Apptainer** with
> `tools-location = dftools`. Includes the controlled-probe data, the exonerated layers, the
> live-verified Questa licensing facts, and the decided fix directions. Companion to
> [dftools-exec-latency.md](dftools-exec-latency.md) (per-exec constant overhead) and
> [ctrl-c-cancellation.md](ctrl-c-cancellation.md) (the signal-wrapper layer of the chain).

## Bottom line

The flakiness is **timeout-shaped resource starvation, not infrastructure failure**. Across ~200
controlled concurrent launches of the full chain (`wsl.exe → apptainer exec <sif> <tool>`) — up to
24-way fan-out, cold-boot fan-out immediately after `wsl --shutdown`, and fan-out into the Win10
idle-teardown window — there were **zero hard failures**. But with the host CPU saturated the way a
real `sbtn test` saturates it (the sbt JVM elaborating on all cores), a single containerized
`ghdl -a` of AES inflates from **3.9 s to 37–74 s**, blowing munit's default 30 s per-test timeout
every time. That is the entire "13 lib suites fail with 30 s munit timeouts in dftools mode"
symptom: a scheduling accident, "flaky" only because it depends on how many suites happen to be in
their tool phase at once.

The rare *hard* failures have separate, identified causes: the Win10 8-second VM idle-teardown
(the historical `GHDL exit 127` / vanished `/tmp/dfhdl-signal-wrapper.sh`, since mitigated by the
per-call recheck in `DFToolsImage.wrapperPath`), and Questa's tool-enforced one-session limit
(exit 12 on a second concurrent `vsim`).

## Test-side concurrency model (what actually runs at once)

- Nothing in `build.sbt` sets `fork`, so **all test suites across all subprojects run inside the
  one sbt server JVM**, with sbt's default task parallelism (~#logical CPUs, here 16). Every tool
  process is spawned from that one JVM — which is what makes a JVM-level permit registry viable.
- `FullCompileSpec` = 5 dialect tests, each `compile` (heavy JVM work) + **lint with every
  available linter**: verilator/iverilog/ghdl/nvc (dftools images) *plus* Questa `vlog`/`vcom` and
  Vivado `xvlog`/`xvhdl`, which have no DFTools image and transparently run locally even in
  dftools mode. So one munit test = JVM compile + 3–4 tool execs, against a 30 s default budget.
- The ips sim specs (`InteractiveSimSpec`, `VgaMonitorSimSpec`) already override
  `munitTimeout = 5 min`; the lib tool-running suites do not (yet).

## Probe methodology

PowerShell-side process fan-out (`System.Diagnostics.Process`, per-call latency =
`ExitTime - StartTime`; see the exec-latency devdoc for why Git Bash must NOT be used to time
`wsl.exe`). Real DFTools images, the real committed AES files from
`sandbox/FullCompileSpec/dfhdl.AES.CipherSpecNoOpaques`, cwd on `/mnt/c` (9p) exactly as DFHDL
runs tools. "Loaded host" = 16 busy-spin processes emulating the test JVM. N processes launched
back-to-back, all awaited; times below are min/median/max of the N.

## Measurements (2026-08-03)

**The chain itself scales cleanly when idle** — zero failures at every N:

| Workload (idle host)                          | N=1    | N=8    | N=16   | N=24   |
| --------------------------------------------- | ------ | ------ | ------ | ------ |
| `wsl -e /bin/true`                             | 88 ms  | 190 ms | 316 ms | 433 ms |
| `apptainer exec <sif> /bin/true`               | 273 ms | 347 ms | 542 ms | 685 ms |
| `apptainer exec <sif> iverilog -V`             | 244 ms | 356 ms | 651 ms | —      |
| in-image iverilog compile, 9 files, 9p cwd     | 300 ms | 326 ms | 580 ms | —      |
| same, ext4 cwd                                 | 218 ms | 287 ms | 595 ms | —      |

**Real lint workloads, idle vs saturated host** (medians; zero hard failures in all cells):

| Workload                          | idle N=1 | idle N=8 | idle N=16 | LOADED N=8 | LOADED N=16 |
| --------------------------------- | -------- | -------- | --------- | ---------- | ----------- |
| `ghdl -a --std=08` AES (9p cwd)   | 3.9 s    | 5.4 s    | 9.3 s     | **38.4 s** | **71.2 s**  |
| `verilator --lint-only` AES (9p)  | 0.8 s    | 1.1 s    | 1.7 s     | 9.7 s      | 17.7 s      |

Decomposition of the 3.9 s solo `ghdl -a`:

| Layer                                        | time   |
| -------------------------------------------- | ------ |
| `apptainer exec <sim-llvm> ghdl --version`   | ~0.26 s warm (0.7 s first) |
| `ghdl -a` AES, ext4 cwd (in-image)           | 3.3 s  |
| `ghdl -a` AES, 9p cwd (in-image)             | 3.9 s (9p adds ~0.6 s idle) |
| `ghdl -a` AES, **local Windows GHDL**        | 4.6 s (container is *not* slower solo) |

So GHDL's LLVM-backend analysis is simply ~3–5 s of real work everywhere; the container adds
little when idle. The ×8–10 blowup under load is the documented WSL2 interaction
([microsoft/WSL#6891](https://github.com/microsoft/WSL/issues/6891): `/mnt/*` 9p filesystem ops
degrade **17–125×** when concurrent CPU-bound processes run) compounding with vCPU
double-scheduling — the VM's 16 vCPUs are host threads competing with the test JVM for the same
16 logical CPUs.

**Event probes** (the races that were suspected but did not reproduce):

- Cold-boot fan-out: `wsl --shutdown`, then 16 concurrent AES `ghdl -a` — all pass, 18.3 s wall
  (boot amortized across the fan-out).
- Teardown-window fan-out: one exec, ~8 s idle gap (the Win10 teardown window), then 16-way
  fan-out — all pass, 9.8 s wall.
- Bounding tool concurrency does **not** fix the latency: the same 16 ghdl jobs run 4-at-a-time
  under the same fixed host load took 54–128 s *per job* (311 s wall). Under a saturated CPU the
  binding resource is the CPU either way — a concurrency cap is a tool for **licenses and VM
  memory**, not for speed.

## Root causes, ranked

1. **CPU starvation × 9p degradation → munit 30 s timeouts** (dominant; fully reproduced above).
   Not a bug in any layer; an unbudgeted timeout. Affects dftools mode much harder than local
   because of WSL#6891 and VM double-scheduling.
2. **Win10 WSL platform hazards** (documented, intermittent, cause the rare hard failures):
   - The VM tears down **~8 s** after the last WSL process exits, and the `vmIdleTimeout` knob is
     **Windows 11 only** — on Win10 the window cannot be lengthened
     ([wsl-config docs](https://learn.microsoft.com/en-us/windows/wsl/wsl-config)). Teardown wipes
     `/tmp` on next boot: this was the historical `GHDL exit 127` (missing signal wrapper), fixed
     by the per-call `test -f` recheck in `DFToolsImage.wrapperPath`.
   - `wslservice.exe` wedge: all `wsl.exe` launches funnel through one COM service with known
     indefinite-hang modes on 19045 ([WSL#8628](https://github.com/microsoft/WSL/issues/8628),
     [WSL#11067](https://github.com/microsoft/WSL/issues/11067)). Cure: kill `wslservice.exe`.
   - 9p channel death: once the `/mnt/c` 9P connection breaks, every container gets EIO until
     `wsl --shutdown` ([WSL#12824](https://github.com/microsoft/WSL/issues/12824), open).
   - Apptainer squashfuse 2 s mount deadline: slow first SIF read under contention → "failed to
     mount ... in 2s", fails once / retry succeeds
     ([apptainer#949](https://github.com/apptainer/apptainer/issues/949)).
3. **Questa single-session enforcement** (verified live; see below): a second concurrent `vsim`
   fails fast — matches the recorded ips Interactive/VgaMonitor questa failures in full runs.
4. **Cold-cache double-pull race in `DFToolsImage.resolve`**: `TrieMap.getOrElseUpdate` is atomic
   in insertion but **not mutually exclusive in evaluation**, so two suites needing the same
   not-yet-downloaded image can both `apptainer pull` to the same destination; two JVMs have no
   lock at all (precedent: [singularity#5020](https://github.com/apptainer/singularity/issues/5020)).
   Narrow (cold cache + concurrent first use) but corrupts a SIF when it hits.
5. **VM memory** (plausible, not observed): no `.wslconfig` on this machine, so the VM gets the
   default 50% of host RAM (15.6 GiB); N concurrent verilator g++ builds could trip the in-VM OOM
   killer, which kills tools silently.

**Exonerated by the probes:** concurrent `wsl.exe` session creation, concurrent `apptainer exec`
of one SIF (squashfuse), light/medium 9p I/O at 16-way, VM cold-boot under fan-out, the
teardown-window race (didn't trip here, though the /tmp-wipe consequence is real), user-namespace
and FUSE-mount count limits (kernel defaults are orders of magnitude above 16).

## Questa licensing facts (verified live on this machine, FSE 2023.3)

- The Intel FPGA **Starter** Edition license is **node-locked, uncounted**
  (`INCREMENT intelqsimstarter mgcld ... uncounted HOSTID=<mac>`), and the **tool itself enforces
  one session per machine**: a second concurrent `vsim` fails after the vopt phase with exit code
  **12** and `"only one session is allowed to run on an uncounted nodelocked license"`. It never
  queues. (Exit 4 is the separate "invalid license environment" case.)
- **A license pre-check is impossible here**: `lmutil lmstat` errors instantly with
  `No SERVER lines in license file (-13,66)` — there is no server to query. Where a server does
  exist, check-then-run is TOCTOU-racy anyway.
- **`vlog`/`vcom` need no license on FSE** (verified with a bogus license path) — the
  FullCompileSpec Questa lints can stay fully parallel. Only `vsim` (simulation) is the 1-seat
  resource.
- Floating-license Questa **queues by default** (only knob: `-lic_noqueue`, or `License = noqueue`
  in `modelsim.ini` `[vsim]`). No "force queue" environment variable exists
  (`MGLS_LICENSE_QUEUE` etc. are myths — absent from the Siemens licensing manual).

## Fix directions

1. **Realistic `munitTimeout` for tool-running lib suites** — IMPLEMENTED: `FullCompileSpec` (the
   base of all docExamples + AES suites) now overrides to 6 min, and the ips sim specs went from
   5 to 6 min (a queued questa run now also waits for the execution permit, item 2). Documented
   reason: one loaded-host lint is 37–74 s (table above), and a test runs several. This alone
   should make dftools full-test runs green, with no runner limits.
2. **Execution permits in `Tool.scala`** — IMPLEMENTED: a tool declares `maxConcurrentExecs`
   (default `Int.MaxValue` = unbounded, zero overhead) and `execPermitKey` (default `toolName`);
   `Tool.exec` takes a JVM-wide fair-semaphore permit around exactly the process lifetime, with a
   log line when it has to wait and Ctrl+C cancellation while queued through both delivery
   channels (an sbt job cancellation interrupts the blocked `acquire` directly; a raw console
   SIGINT fires a temporary INT handler that interrupts the waiting thread). `QuestaSimCommon` sets
   count 1 under the shared key `"QuestaSim"`, so vlog, vcom, and the vsim run they precede are
   serialized across every suite (all test tool launches share the one sbt server JVM). The
   mechanism is deliberately general: it can later bound overall system usage during tests (e.g.
   a shared key for the heavy dftools simulators). Rejected alternatives: limiting the questa
   *tests* (the limit must span lib + ips + app runs; test tagging doesn't reach that), and the
   lmstat pre-check (impossible on FSE, racy everywhere). Possible future hardening, not
   implemented: a cross-process `FileChannel.tryLock` (a second sbt session / standalone DFApp)
   and a bounded retry keyed on the `"only one session is allowed"` message (a Questa GUI the
   user left open is the one collision no DFHDL-side permit can see).
3. **`DFToolsImage.resolve` double-pull race** — IMPLEMENTED: `handle` resolves a cold image
   under a per-image lock (in-JVM exclusion; `TrieMap.getOrElseUpdate` alone is not mutually
   exclusive), and the download itself goes to a private per-process staged name, is
   sha-verified, and is then published by an atomic `mv -f` — so a concurrent *process* can
   neither observe nor clobber a partial SIF, and a corrupt download is never visible.
4. **WSL hygiene** (cheap, optional): pin an explicit `.wslconfig` (memory/processors); hold one
   keepalive WSL session open for the duration of a test run so the Win10 8 s teardown can never
   fire mid-suite (eliminates the whole /tmp-wipe class at the source); know the wslservice-wedge
   symptom (all wsl.exe hang, `wsl --version` still works → kill `wslservice.exe`).
5. **Non-fixes**: bounding global tool concurrency for *speed* (measured: does not help under a
   saturated host); `apptainer instance` / persistent WSL session shave only the ~160 ms/exec
   constant (see the exec-latency devdoc) — irrelevant next to the seconds-scale dominant term.

## Environment

- Windows 10 Pro 19045, 16 logical CPUs, 32 GB RAM, **no `.wslconfig`** (VM: 15.6 GiB / 16 vCPU /
  4 GiB swap by default). WSL 2.7.10, kernel 6.18.33.2-microsoft-standard-WSL2, WSLg 1.0.73.
- Apptainer 1.5.3 (system install at `/usr/bin/apptainer` in the Ubuntu distro), non-setuid.
- DFTools per-tool images from `~/.scalapptainer/images` (sim-llvm ≈ 177 MB, sim-verilator ≈
  155 MB, sim-iverilog ≈ 33 MB).
- Questa Intel FPGA Starter Edition 2023.3 (`C:\intelFPGA_lite\23.1std\questa_fse\win64`).
- Numbers are machine-specific; the methodology above reproduces them.

## Sources

[WSL#6891](https://github.com/microsoft/WSL/issues/6891) (concurrent CPU load degrades FS ops
17–125×), [WSL#8628](https://github.com/microsoft/WSL/issues/8628) /
[WSL#11067](https://github.com/microsoft/WSL/issues/11067) (wslservice hangs),
[WSL#12824](https://github.com/microsoft/WSL/issues/12824) (9p EIO),
[WSL#4197](https://github.com/microsoft/WSL/issues/4197) /
[WSL#5103](https://github.com/microsoft/WSL/issues/5103) (9p performance),
[wsl-config docs](https://learn.microsoft.com/en-us/windows/wsl/wsl-config) (defaults; 8 s rule;
`vmIdleTimeout` Win11-only), [apptainer#949](https://github.com/apptainer/apptainer/issues/949)
(squashfuse mount deadline), [apptainer#665](https://github.com/apptainer/apptainer/issues/665)
(squashfuse single-thread decompression),
[singularity#5020](https://github.com/apptainer/singularity/issues/5020) (concurrent pull race),
[Questa SIM Command Reference 2024.2](https://ww1.microchip.com/downloads/aemDocuments/documents/FPGA/swdocs/questasim/questa_sim_ref_2024_2.pdf)
(`-lic_noqueue`, p. 1038),
[Questa SIM User's Manual 2024.2](https://ww1.microchip.com/downloads/aemDocuments/documents/FPGA/swdocs/questasim/questa_sim_user_2024_2.pdf)
(`License` ini var p. 1642; exit codes pp. 1860–1862),
[Siemens EDA Licensing Manual 2024.2](https://ww1.microchip.com/downloads/aemDocuments/documents/FPGA/swdocs/questasim/sw_siemens_licensing_eda_2024_2.pdf),
[Intel doc 683472](https://www.intel.com/content/www/us/en/docs/programmable/683472/22-4/and-software-license.html)
(Starter license), [VUnit#877](https://github.com/VUnit/vunit/issues/877) (parallel Questa
fragility).
