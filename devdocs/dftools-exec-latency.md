# DFTools Execution Latency — Measurements

> Per-invocation latency that the **DFTools** path (Apptainer image, `tools-location = dftools`) adds
> to every external EDA tool execution, versus a **local** install, on **Windows + WSL2**. Includes a
> decomposition (wsl.exe launch vs. apptainer container start vs. tool) and the effect of using a
> persistent `apptainer instance` instead of a per-call `apptainer exec <sif>`.

## Bottom line

On Windows + WSL2, DFTools adds **~160 ms of fixed infrastructure overhead per tool *process launch***
(≈100 ms `wsl.exe` spawn + ≈60 ms apptainer container start), before the tool does any work. A local
launch is ~5–10 ms. The signal-trapping cancellation wrapper costs ≈0 ms.

This is **per process launch, not per simulation step**. A `simulate` run spawns the tool a handful of
times (lint → sim-prep → sim-run); a FOSS `build` spawns yosys + nextpnr + packer. So the real added
wall-clock ≈ `~0.16 s × (number of tool execs)` — typically a few tenths of a second per run, usually
invisible against a multi-second compile/sim.

Using a persistent **`apptainer instance`** roughly **halves the apptainer portion** (saves ~110 ms per
real tool exec), but the `wsl.exe` spawn (~100 ms) then becomes the dominant residual floor.

## Methodology & a measurement gotcha

- Timings are wall-clock around the host process launch — i.e. what DFHDL's JVM `ProcessBuilder` +
  `waitFor` actually experiences. The faithful host-side tool is **PowerShell `Measure-Command`**
  (native `CreateProcess`/wait).
- **Do not use Git Bash / MSYS2 to time `wsl.exe`.** Its `wsl.exe` wrapper returns when the console
  relay closes, *not* when the in-VM process exits, so every measurement saturates at a bogus ~130 ms
  regardless of payload. The PowerShell numbers and the in-WSL numbers agree; the Git Bash ones don't.
- "In-WSL" rows run the loop inside a single already-attached `wsl.exe` session (one spawn, timed with
  `date +%s%N`), isolating the apptainer layer from the per-call `wsl.exe` launch.
- Each figure is an average of 8–12 iterations after a warmup run.

## Host-side, per-call (the real DFHDL path)

Each call spawns a fresh `wsl.exe` (as DFHDL does today), running
`apptainer exec <image> <tool> …` through the in-VM signal wrapper.

| Layer (cumulative)                                   | avg    | adds                              |
| ---------------------------------------------------- | ------ | --------------------------------- |
| local `verilator_bin.exe -version`                   | ~9 ms  | — (baseline)                      |
| `wsl.exe -e /bin/true`                               | ~100 ms| **+~100 ms** `wsl.exe` launch     |
| `wsl.exe -e apptainer exec <sif> /bin/true`          | ~167 ms| **+~60 ms** apptainer container start |
| `wsl.exe -e apptainer exec <sif> verilator -version` | ~311 ms| +~150 ms verilator's own startup  |
| same, via the signal-trapping wrapper                | ~320 ms| **+~1 ms** wrapper (negligible)   |

Observations:
- The overhead is **constant across tools/images** — apptainer mounts the SquashFS lazily, so image
  size (33 MB iverilog vs. 247 MB verilator) doesn't change startup.
- `wsl.exe` launch (~100 ms) is the dominant term and is **Windows-specific**. On native Linux there is
  no WSL hop, leaving only the ~60 ms apptainer start. macOS (Lima) would resemble Windows.

## `apptainer instance` vs. per-call `apptainer exec <sif>`

A persistent instance (`apptainer instance start <sif> <name>`, then `apptainer exec instance://<name>
…`) does the SquashFS mount + namespace setup **once**, so each `exec` just attaches.

In-WSL (isolates the apptainer layer, N=12):

| command              | `exec <sif>` | `exec instance://` | saved       |
| -------------------- | ------------ | ------------------ | ----------- |
| `verilator -version` | 266 ms       | 143 ms             | **−123 ms** |
| `/bin/true`          | 110 ms       | 58 ms              | **−52 ms**  |

Host-side (fresh `wsl.exe` per call, N=12):

| command              | `exec <sif>` | `exec instance://` |
| -------------------- | ------------ | ------------------ |
| `verilator -version` | 311 ms       | **200 ms**         |
| `/bin/true`          | 167 ms       | 130 ms             |
| `wsl -e /bin/true` (floor) | —      | 89 ms              |

The instance win is larger for real tools (−123 ms) than for `/bin/true` (−52 ms) because per-call
`exec <sif>` re-mounts the image and rebuilds the namespace every time, which the instance amortizes.

## Levers (in priority order)

1. **`apptainer instance` (moderate change, ~110 ms/exec win).** Lazily start one instance per needed
   image, `exec instance://…` per tool call, stop on JVM shutdown. Costs: a one-time `instance start`
   (~one container start), plus lifecycle management — stale instances after a crash, unique names per
   sandbox, and the `.dfhdl-cancel` / signal-wrapper kill path must target the instance's process tree.
   Scalapptainer would need an instance API (it currently models `exec <sif>` only).
2. **Persistent WSL session (bigger change, removes the ~100 ms floor).** Keep one long-lived `wsl.exe`
   and feed it commands instead of spawning a fresh one per exec. Combined with #1 this approaches the
   in-WSL numbers (~58 ms for a trivial exec). More invasive and complicates the Ctrl+C interop kill.

Given a full DFHDL flow does only a handful of execs, the absolute saving from #1 is a few hundred ms
per run — real but modest against multi-second compiles. Worth doing only if many short tool calls are
expected.

## Environment

- Host: Windows 10 Pro (19045), Apptainer 1.5.2 inside WSL2.
- Images: `~/.scalapptainer/images/dftools-*-v*-linux-x64.sif` (per-tool DFTools v0.x images).
- DFTools Windows exec path: `wsl.exe -e bash <signal-wrapper> <jvmPid> apptainer exec <sif> <tool> …`
  (see [ctrl-c-cancellation.md](ctrl-c-cancellation.md)).
- Numbers are machine-specific; re-measure on other hardware. The PowerShell/in-WSL timing scripts used
  here are reproducible from the tables above.
