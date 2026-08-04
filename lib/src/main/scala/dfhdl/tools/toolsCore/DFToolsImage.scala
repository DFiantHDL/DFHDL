package dfhdl.tools.toolsCore
import scalapptainer.*
import dfhdl.internals.osIsWindows

/** Resolves and caches the per-tool DFTools Apptainer images that DFHDL runs the external EDA tools
  * from when `tools-location` is `dftools`.
  *
  * The tools are split across several images (clustered by shared dependency); [[imageFor]] maps an
  * in-image executable name (and, for yosys, the backend dialect) to its image. Each image is
  * resolved independently:
  *   1. the dev/test override `-Ddfhdl.dftools.sif.<image>=<path>` (a backend-accessible .sif) —
  *      used by DFTools CI to validate freshly-built images before publishing;
  *   2. otherwise the published per-image release asset for the current arch, downloaded into
  *      Scalapptainer's image cache once and reused.
  *
  * The `apptainer exec` invocation itself is built ([[execArgv]]) and spawned by [[Tool.exec]] so
  * it shares DFHDL's stdout/cancellation handling.
  */
object DFToolsImage:
  /** The DFTools release this DFHDL build targets, set from `build.sbt` (`dftoolsVersion`) via
    * lib's generated `dftools.properties` resource. Bump it there when adopting a new DFTools
    * release.
    */
  val version: String =
    val props = new java.util.Properties()
    // `lib` owns this file (kept separate from core's `version.properties` so the two are
    // decoupled). Close the stream: a leaked handle blocks the build from re-copying the resource
    // on Windows (AccessDenied during copyResources).
    val inputStream = getClass.getClassLoader.getResourceAsStream("dftools.properties")
    try props.load(inputStream)
    finally inputStream.close()
    props.getProperty("dftools.version")
  private val repo = "DFiantHDL/DFTools"

  /** Map an in-image executable name to its DFTools image, or `None` if no DFTools image provides
    * it (proprietary tools such as Questa/Vivado/Quartus/Diamond/Gowin). Yosys depends on the
    * backend dialect (VHDL synthesis loads the ghdl frontend, hence `synth-vhdl`).
    */
  def imageForOpt(exec: String, vhdl: Boolean): Option[String] = exec match
    case "yosys" => Some(if (vhdl) "synth-vhdl" else "synth-verilog")
    case "eqy"   => Some("synth-verilog")
    case "nextpnr-ecp5" | "nextpnr-himbaechel" | "ecppack" | "gowin_pack" => Some("pnr")
    case "ghdl" | "nvc"                                                   => Some("sim-llvm")
    case "verilator" | "verilator_bin"                                    => Some("sim-verilator")
    case "iverilog" | "vvp"                                               => Some("sim-iverilog")
    case "surfer"                                                         => Some("wavegen")
    case "openFPGALoader"                                                 => Some("program")
    case _                                                                => None

  /** Like [[imageForOpt]] but throws for an unsupported tool. Callers reach this only after
    * [[Tool.usesDFTools]] has confirmed the tool has an image.
    */
  def imageFor(exec: String, vhdl: Boolean): String =
    imageForOpt(exec, vhdl).getOrElse(
      throw new IllegalArgumentException(s"no DFTools image for tool '$exec'")
    )

  /** Dev/test override: run a given image from a specific local .sif (path valid in the backend).
    */
  def overrideSif(image: String): Option[String] =
    Option(System.getProperty(s"dfhdl.dftools.sif.$image")).map(_.trim).filter(_.nonEmpty)

  /** `linux-x64` | `linux-arm64`, per the backend's reported machine type. */
  private lazy val archTag: String =
    Apptainer.backend.runShell("uname -m").out.trim match
      case "aarch64" | "arm64" => "linux-arm64"
      case _                   => "linux-x64"

  /** Parsed once from the build-time–bundled `dftools.lock.json` (the DFTools release lockfile for
    * [[version]]): `image -> arch -> (sha256, immutable asset name)`. The lockfile is the umbrella
    * tag's only version-keyed artifact; everything below resolves and caches per-image by sha256,
    * so a DFTools version bump that did not change a given image keeps its digest and asset name,
    * and the image is never re-downloaded just because the tag moved.
    */
  private lazy val lock: Map[String, Map[String, (String, String)]] =
    val in = getClass.getClassLoader.getResourceAsStream("dftools.lock.json")
    require(in != null, "dftools.lock.json resource missing from the DFHDL build (rebuild `lib`)")
    try
      val js = ujson.read(new String(in.readAllBytes(), java.nio.charset.StandardCharsets.UTF_8))
      require(
        js("tag").str == version,
        s"dftools.lock.json tag '${js("tag").str}' != dftoolsVersion '$version' " +
          "(rebuild `lib` to refresh the bundled lockfile)"
      )
      js("images").obj.view.mapValues { arches =>
        arches.obj.view.mapValues(e => (e("sha256").str, e("asset").str)).toMap
      }.toMap
    finally in.close()

  /** `(sha256, asset)` for an image on the current arch, from the bundled lockfile. */
  private def lockEntry(image: String): (String, String) =
    lock
      .getOrElse(
        image,
        throw new IllegalStateException(s"image '$image' absent from dftools.lock.json")
      )
      .getOrElse(
        archTag,
        throw new IllegalStateException(
          s"image '$image' has no $archTag asset in dftools.lock.json"
        )
      )

  private def assetUrl(asset: String): String =
    s"https://github.com/$repo/releases/download/$version/$asset"

  private val handles = scala.collection.concurrent.TrieMap.empty[String, ApptainerImage]
  // Per-image resolve locks. `TrieMap.getOrElseUpdate` is atomic in what it stores but NOT
  // mutually exclusive in evaluating the default, so without a lock two threads first-using the
  // same cold image would both run `apptainer pull` to the same destination. Racing on the lock
  // objects themselves is harmless: both racers receive the single stored instance.
  private val resolveLocks = scala.collection.concurrent.TrieMap.empty[String, Object]

  /** The resolved handle for an image (memoized); downloads the release asset on first use. The
    * warm path is lock-free; a cold image resolves under a per-image lock so concurrent first uses
    * pull once (`resolve`'s staged download additionally covers a concurrent *process*).
    */
  def handle(image: String): ApptainerImage =
    handles.getOrElse(
      image,
      resolveLocks.getOrElseUpdate(image, new Object).synchronized {
        handles.getOrElseUpdate(image, resolve(image))
      }
    )

  private def resolve(image: String): ApptainerImage =
    overrideSif(image) match
      case Some(p) => Apptainer.image(p)
      case None    =>
        val (sha, asset) = lockEntry(image)
        // The asset name is immutable and content-addressed (it embeds the digest), so its presence
        // in the backend image cache means we already have exactly these bytes — a DFTools version
        // bump that didn't change this image resolves to the same asset and skips the pull entirely.
        // `Apptainer.pull` reuses an existing dest (backend `test -f`) without re-downloading.
        val dest = s"${Apptainer.imagesDir}/$asset"
        if (Apptainer.image(dest).exists) Apptainer.image(dest) // already cached — reuse silently
        else
          // Missing: announce the download (image + DFTools version) so the wait is explained even
          // when there is no live progress bar (no TTY — e.g. under sbtn). `interactive = true` also
          // lets a terminal-attached run surface Apptainer's own download progress bar. Then verify
          // the fresh bytes and report completion. Both messages are gated on the cache miss so the
          // common warm-cache path stays noise-free.
          println(s"[dftools] downloading image '$image' ($version)...")
          // Download to a private per-process name, verify, then atomically rename into place: the
          // shared dest is thus never visible half-written, so a concurrent DFHDL *process* (e.g. a
          // second sbt session; in-JVM racers are already excluded by `handle`'s per-image lock)
          // can neither pick up nor clobber a partial download. Racing winners hold identical
          // bytes (the asset name is content-addressed), so `mv -f` in either order is safe, and
          // verifying before publishing means a corrupt download is never visible to anyone.
          val staged = s"$dest.${ProcessHandle.current().pid()}.tmp"
          try
            Apptainer.pull(assetUrl(asset), dest = Some(staged), interactive = true)
            verifySha256(staged, sha) // verify the fresh bytes, backend-side, before publishing
            Apptainer.backend.runShell(s"mv -f '$staged' '$dest'").throwIfFailed()
          finally
            // no-op on success (the rename consumed it); drops the partial file a failed pull
            // can leave behind (a failed verify already removed it)
            Apptainer.backend.runShell(s"rm -f '$staged'")
          println(s"[dftools] image '$image' ready")
          Apptainer.image(dest)
        end if

  /** Verify a freshly pulled SIF against its expected sha256. The file lives in the backend (a WSL
    * VM on Windows), so hash it there — a host-side digest would read a non-existent path. On a
    * mismatch the corrupt download is removed and the failure is surfaced loudly.
    */
  private def verifySha256(dest: String, expected: String): Unit =
    val got = Apptainer.backend
      .runShell(s"sha256sum '$dest'")
      .out
      .trim
      .split("\\s+")
      .headOption
      .getOrElse("")
    if (got != expected)
      Apptainer.backend.runShell(s"rm -f '$dest'")
      throw new IllegalStateException(
        s"sha256 mismatch for DFTools image '$dest': got '$got', expected '$expected' " +
          "(corrupt download removed; retry)"
      )

  /** Whether the given image is resolvable (present locally / overridden / downloadable). A resolve
    * failure (no container runtime, blocked unprivileged user namespaces, a corrupt or absent
    * asset) is reported before returning false, so it is distinguishable from an image that is
    * simply not configured — otherwise the only downstream symptom is a misleading "could not be
    * found in its DFTools image".
    */
  def isAvailable(image: String): Boolean =
    try handle(image).exists
    catch
      case e: Throwable =>
        println(s"[dftools] image '$image' unavailable: $e")
        false

  /** Run a command inside the image and return its combined stdout+stderr (trimmed). Used for
    * version probes in `dftools` mode, where the tool lives in the image rather than on the host
    * PATH. Some tools print their version banner to stderr, so both streams are returned.
    */
  def probe(image: String, cmd: Seq[String]): String =
    val r = handle(image).exec(cmd*)
    s"${r.out}\n${r.err}".trim

  /** A signal-trapping bash wrapper run inside the backend VM (WSL). Across the WSL2 boundary a
    * console Ctrl+C reaches this in-VM wrapper (verified) but NOT the host JVM, so the JVM-side
    * cancellation never fires under e.g. scala-cli. Crucially, killing the in-VM tool alone does
    * NOT stop the flood — only killing the host `wsl.exe` launcher (which drops its output buffer)
    * does. The wrapper can do exactly that via WSL→Windows interop: its first arg is the host JVM
    * pid, and on a trapped INT/TERM/HUP it runs `wmic.exe` to terminate the `wsl.exe` whose parent
    * is that JVM (i.e. our launcher, `process.wrapped`) — stopping the flood and unblocking the
    * JVM's `waitFor`. It also force-kills the in-VM apptainer/tool subtree (stops new output at the
    * source) and drops a `./.dfhdl-cancel` marker in the (apptainer-mounted) cwd so the JVM can
    * report a clean interrupt. `$1` is the JVM pid; the real command is the remaining `"$@"`.
    */
  private val signalWrapper: String =
    """jpid="$1"; shift; """ +
      """kt(){ local p=$1 c; for c in $(pgrep -P "$p" 2>/dev/null); do kt "$c"; done; kill -KILL "$p" 2>/dev/null; }; """ +
      """cu(){ trap - INT TERM HUP; : > ./.dfhdl-cancel 2>/dev/null; wmic.exe process where "name='wsl.exe' and parentprocessid=$jpid" call terminate >/dev/null 2>&1; [ -n "${child:-}" ] && kt "$child"; exit 130; }; """ +
      """trap 'cu INT' INT; trap 'cu TERM' TERM; trap 'cu HUP' HUP; """ +
      """"$@" & child=$!; wait "$child"; exit $?""" +
      "\n"

  // Absolute path of the wrapper script inside the backend VM.
  private val wrapperVMPath = "/tmp/dfhdl-signal-wrapper.sh"
  private val wrapperLock = new Object

  /** Ensure the signal wrapper is present inside the backend VM, and return its in-VM path.
    *
    * We cannot pass the script inline as a `bash -c <script>` argument: across `wsl.exe` it is
    * re-quoted three times (ProcessBuilder -> wsl.exe -> bash) and its `$(...)`/quotes get
    * corrupted. Instead we install it by piping the content through stdin to `tee` (so it never
    * appears on a command line), then invoke it by path with only simple tokens.
    *
    * Presence is re-checked on every call rather than memoized per JVM: the backend VM is torn down
    * whenever it goes idle (WSL2 shuts down ~60s after its last process exits) and systemd's
    * tmpfiles cleaner wipes `/tmp` on each boot, so an "installed once" flag goes stale mid-session
    * in a long-lived JVM (the sbt server), and the next dftools command dies with `bash:
    * /tmp/dfhdl-signal-wrapper.sh: No such file` (exit 127). The check is one cheap `test -f`,
    * negligible next to the apptainer run it wraps.
    *
    * Thread-safe: concurrent tool invocations serialize here, and the script is published by an
    * atomic rename from a private temp name. Writing the final path in place would truncate it
    * under a wrapper that a concurrent exec is still running (bash reads a script file lazily, so
    * it would read garbage); a rename swaps the directory entry and leaves that inode intact.
    */
  private def wrapperPath(): String = wrapperLock.synchronized {
    val installed =
      os.proc(Apptainer.backend.wrapApptainer("test", Seq("-f", wrapperVMPath)))
        .call(stdout = os.Pipe, stderr = os.Pipe, check = false).exitCode == 0
    if (!installed)
      val stagedVMPath = s"$wrapperVMPath.${ProcessHandle.current().pid()}.tmp"
      val tmp = os.temp(prefix = "dfhdl-signal-wrapper", suffix = ".sh")
      os.write.over(tmp, signalWrapper)
      try
        os.proc(Apptainer.backend.wrapApptainer("tee", Seq(stagedVMPath)))
          .call(stdin = tmp, stdout = os.Pipe, stderr = os.Pipe)
        os.proc(Apptainer.backend.wrapApptainer("mv", Seq("-f", stagedVMPath, wrapperVMPath)))
          .call(stdout = os.Pipe, stderr = os.Pipe)
      finally os.remove.all(tmp)
    wrapperVMPath
  }

  /** Build the host argv for `apptainer exec [opts] <image> <containerCmd...>`, optionally
    * forwarding X11 (for GUI tools such as the waveform viewer) and a set of environment variables.
    *
    * The `env` entries are emitted as `--env KEY=VAL` apptainer flags rather than set on the host
    * process: the in-image command's environment must carry e.g. the foreign-IP runtime lib path
    * (`LD_LIBRARY_PATH`) and a viewer rendezvous (`VGA_MONITOR_STREAM`), and `--env` flags are
    * plain argv tokens that survive the `wsl.exe` boundary on Windows (host env vars would need a
    * `WSLENV` allow-list to cross). Path-like values may be relative — they resolve against the
    * in-container cwd (the mounted `$PWD`), which is the exec dir.
    */
  def execArgv(
      image: String,
      containerCmd: Seq[String],
      withX11: Boolean,
      env: Map[String, String] = Map.empty
  ): Seq[String] =
    val base = if (withX11) handle(image).withX11() else handle(image)
    val opts0 = if (env.isEmpty) base.options else base.options.env(env.toSeq*)
    // Skip Apptainer's default `/etc/resolv.conf` bind mount. Our sim/synth containers are fully
    // offline and every foreign-IP endpoint is a literal `127.0.0.1:port`, so none of them need DNS.
    // On WSL2 `/etc/resolv.conf` is a symlink into the shared tmpfs `/mnt/wsl`, which is
    // intermittently unpropagated in a freshly-spawned `wsl.exe` session under systemd; Apptainer
    // then stats the mount source, finds the whole subtree momentarily absent, and aborts with a
    // random `FATAL: mount source /etc/resolv.conf doesn't exist` (see apptainer#2931 / PR #3284 —
    // the shipped symlink-directory fix doesn't cover this WSL whole-subtree-absent case). Dropping
    // the useless mount removes the dependency entirely. NB: use the absolute-path token
    // `/etc/resolv.conf` — the short `resolv.conf` key is rejected ("unknown mount type") on the
    // Apptainer we target; PR #3284 added `--no-mount` support specifically for the path form.
    val opts = opts0.arg("--no-mount", "/etc/resolv.conf")
    val cmd = commands.ExecCommand(base.ref, containerCmd, opts)
    if (osIsWindows)
      // Run apptainer through the signal-trapping wrapper installed in the VM. `wrapApptainer` just
      // prepends the backend command prefix and treats its first arg as the in-VM program, so the
      // in-VM command is `bash <wrapper> <jvmPid> <apptainer> exec ...` — the wrapper consumes the
      // JVM pid (to identify and kill our host `wsl.exe` on cancel) and runs apptainer as `"$@"`.
      // Every token here is simple (no spaces/quotes), so nothing is mangled by wsl.exe.
      val jvmPid = ProcessHandle.current().pid().toString
      Apptainer.backend.wrapApptainer(
        "bash",
        wrapperPath() +: jvmPid +: Apptainer.apptainerPath +: cmd.args
      )
    else
      Apptainer.backend.wrapApptainer(Apptainer.apptainerPath, cmd.args)
  end execArgv
end DFToolsImage
