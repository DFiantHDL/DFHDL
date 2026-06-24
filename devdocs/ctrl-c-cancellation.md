# DFTools Ctrl+C Cancellation — Design & Findings

> How DFHDL cancels (Ctrl+C) a running external EDA tool, with emphasis on the hard case: a tool
> that **floods stdout** while running **inside the DFTools Apptainer image** (`tools-location =
> dftools`) on **Windows + WSL2**. Confirmed working for local & dftools tools under both `sbtn` and
> `scala-cli`.

## The problem

A runaway/verbose tool (e.g. a verilator simulation `obj_dir/V<top>`) floods stdout. Pressing Ctrl+C
did not stop it — it "took forever", and under `dftools` on Windows it had to be killed manually in
WSL. Two independent issues, with different root causes per launcher.

## How cancellation reaches us (per launcher)

DFHDL spawns the tool from `Tool.exec`. The way a Ctrl+C arrives differs:

- **`sbt` / standalone JVM:** a POSIX/Windows **SIGINT** is delivered to the JVM → our `sun.misc`
  INT handler fires.
- **`sbtn`:** the run executes on a detached sbt **server** JVM; the client forwards the cancel as a
  **`Thread.interrupt()`**, so `process.wrapped.waitFor()` throws `InterruptedException` (no signal).
- **`scala-cli` (and similar) on Windows + dftools:** the launcher **hides Ctrl+C from the app JVM**
  entirely — *no* SIGINT, *no* `Thread.interrupt`, *no* shutdown. We proved this with a diagnostic
  build: the JVM-side INT handler never fired in any run. The console Ctrl+C **does** reach the in-VM
  process though (see below).

## Fix 1 — sbtn flood: output throttle (committed separately)

Under `sbtn` the cancel is forwarded over the client↔server channel. A sustained output **flood
saturates that channel**, so the client never gets to deliver the cancel — the run isn't interrupted.

Fix: **continuously cap** forwarded console output (`Tool.outputThrottle`, ~500 lines/s) so the
channel stays mostly idle and the cancel gets through; plus an **`aborted` flag** so the post-kill
backlog is dropped silently instead of trickling out at the throttled rate. The cap must be
*continuous* — any full-speed burst re-saturates the channel instantly (adaptive/burst variants
failed). Output below the cap is untouched; nothing is dropped.

Once the cancel arrives, `destroyToolTree()` force-kills `process.wrapped` (the host launcher) and
its live descendants.

## Fix 2 — dftools flood on Windows: in-VM signal wrapper + interop kill

Key facts established by experiment:

1. **The JVM never sees the Ctrl+C** under scala-cli (above).
2. The console Ctrl+C **does** reach the in-VM process: WSL forwards **SIGINT** across the boundary.
3. Killing the **in-VM tool alone does not stop the flood** — the host `wsl.exe` relay keeps draining
   its already-buffered ("stacked") output. Only **killing the host `wsl.exe`** drops that buffer.
4. WSL→Windows **interop** lets an in-VM process run Windows executables (`wmic.exe`, `taskkill.exe`).

So the tool is run inside the VM through a small **signal-trapping bash wrapper**
([`DFToolsImage.signalWrapper`](../lib/src/main/scala/dfhdl/tools/toolsCore/DFToolsImage.scala)).
On a trapped INT/TERM/HUP it:

1. **Kills our host `wsl.exe` via interop** —
   `wmic.exe process where "name='wsl.exe' and parentprocessid=<jvmPid>" call terminate`. That target
   is the JVM's direct child (`process.wrapped`); killing it drops the relay's output buffer (stops
   the flood) and EOFs the JVM's pipe (returns `waitFor`). Matching by **parent = JVM pid** is precise
   and race-free: the wrapper runs under a *child* relay, so the wrapper's own relay survives to finish.
2. **Force-kills the in-VM apptainer/tool subtree** (stops new output at the source).
3. **Drops a `.dfhdl-cancel` marker** in the (apptainer-mounted) cwd, shared with the host.

The JVM passes its own pid to the wrapper as the first argument. The wrapper is **installed in the VM
via `tee` (content piped over stdin) and invoked by path** — passing the script inline as a `bash -c`
argument does not survive the triple re-quoting across ProcessBuilder → `wsl.exe` → bash.

JVM side (`Tool.exec`): a daemon **marker watcher** that, on seeing `.dfhdl-cancel`, tears down the
launcher from here too (fallback if the interop kill didn't land) and marks the run interrupted; plus
a single post-`waitFor` marker check so a clean `ToolInterruptedException` ("interrupted by user") is
raised instead of reading the killed launcher's exit code as an error. Normal runs pay no latency
(no marker → instant false).

## Current code (DFHDL working tree, on `dftools`)

- `lib/.../toolsCore/Tool.scala` — `outputThrottle` (committed), `aborted`-drop, `destroyToolTree`,
  the cancel-marker watcher + post-`waitFor` marker check, INT handler + thread-interrupt path.
- `lib/.../toolsCore/DFToolsImage.scala` — `signalWrapper`, `wrapperPath` (tee-install, memoized),
  `execArgv` runs apptainer through the wrapper on Windows (passing the JVM pid).
- `build.sbt` / `DFToolsImage` otherwise unchanged; `scalapptainer 0.5.0` (released).

## Dead ends (reverted — do not redo)

- A Scalapptainer "cancelable run" feature (`wrapCancelable`/`killRunGroup`, `apptainer exec --pid`,
  process-group/session kills, a supervising runner). Misdiagnosis: the kill was never the problem.
  Still sits uncommitted in the Scalapptainer clone at `C:\Users\OronPort\IdeaProjects\Scalapptainer`
  (+ `0.5.0-cancel-local*` ivy artifacts) — discard with `git checkout` there.
- A **daemon output reader** (`os.Pipe` + our own thread) to fix a presumed non-daemon-pumper JVM-exit
  hang — didn't help; the JVM never gets the signal under scala-cli, so nothing JVM-side could.
- A **`.dfhdl-cancel` marker → JVM → kill `wsl.exe`** bridge as the *primary* kill — fragile (relies on
  shared-FS timing). Replaced by the in-VM interop kill; the marker is now only a fallback + status.
- `taskkill /F /T` on the launcher from the JVM — a console Ctrl+C kills the launcher before the JVM
  handler runs, so a kill-time tree walk finds nothing (the relay is already orphaned).

## Gotchas / notes

- Measurement bug: `pgrep -f <text>` matches a process's *output text*; use `pgrep -xc <binname>` on a
  compiled binary to count survivors.
- To test scala-cli you must `publishLocal` DFHDL first — it resolves the published lib, not the sbt
  working tree.
- `wmic` is deprecated (absent on Win11 24H2+); a PowerShell `Get-CimInstance … Stop-Process` fallback
  is the future-proofing if it's ever removed.
