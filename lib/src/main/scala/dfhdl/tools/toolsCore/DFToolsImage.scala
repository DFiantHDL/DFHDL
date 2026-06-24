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
  /** The DFTools release this DFHDL build targets, set from `build.sbt` (`dftoolsVersion`) via the
    * generated `version.properties` resource. Bump it there when adopting a new DFTools release.
    */
  val version: String =
    val props = new java.util.Properties()
    val inputStream = getClass.getClassLoader.getResourceAsStream("version.properties")
    props.load(inputStream)
    props.getProperty("dftools.version")
  private val repo = "DFiantHDL/DFTools"

  /** Map an in-image executable name to its DFTools image, or `None` if no DFTools image provides it
    * (proprietary tools such as Questa/Vivado/Quartus/Diamond/Gowin). Yosys depends on the backend
    * dialect (VHDL synthesis loads the ghdl frontend, hence `synth-vhdl`).
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

  private def assetUrl(image: String): String =
    s"https://github.com/$repo/releases/download/$version/dftools-$image-$archTag.sif"

  private val handles = scala.collection.concurrent.TrieMap.empty[String, ApptainerImage]

  /** The resolved handle for an image (memoized); downloads the release asset on first use. */
  def handle(image: String): ApptainerImage =
    handles.getOrElseUpdate(image, resolve(image))

  private def resolve(image: String): ApptainerImage =
    overrideSif(image) match
      case Some(p) => Apptainer.image(p)
      case None    =>
        val dest = s"${Apptainer.imagesDir}/dftools-$image-$version-$archTag.sif"
        Apptainer.pull(assetUrl(image), dest = Some(dest))

  /** Whether the given image is resolvable (present locally / overridden / downloadable). */
  def isAvailable(image: String): Boolean =
    try handle(image).exists
    catch case _: Throwable => false

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

  // Absolute path of the wrapper script inside the backend VM. We cannot pass the script inline as a
  // `bash -c <script>` argument: across `wsl.exe` it is re-quoted three times (ProcessBuilder ->
  // wsl.exe -> bash) and its `$(...)`/quotes get corrupted. Instead we install it once by piping the
  // content through stdin to `tee` (so it never appears on a command line), then invoke it by path
  // with only simple tokens. Memoized: written once per JVM.
  private lazy val wrapperPath: String =
    val path = "/tmp/dfhdl-signal-wrapper.sh"
    val tmp = os.temp(prefix = "dfhdl-signal-wrapper", suffix = ".sh")
    os.write.over(tmp, signalWrapper)
    try
      os.proc(Apptainer.backend.wrapApptainer("tee", Seq(path)))
        .call(stdin = tmp, stdout = os.Pipe, stderr = os.Pipe)
    finally os.remove.all(tmp)
    path

  /** Build the host argv for `apptainer exec [opts] <image> <containerCmd...>`, optionally
    * forwarding X11 (for GUI tools such as the waveform viewer).
    */
  def execArgv(image: String, containerCmd: Seq[String], withX11: Boolean): Seq[String] =
    val img = if (withX11) handle(image).withX11() else handle(image)
    val cmd = commands.ExecCommand(img.ref, containerCmd, img.options)
    if (osIsWindows)
      // Run apptainer through the signal-trapping wrapper installed in the VM. `wrapApptainer` just
      // prepends the backend command prefix and treats its first arg as the in-VM program, so the
      // in-VM command is `bash <wrapper> <jvmPid> <apptainer> exec ...` — the wrapper consumes the
      // JVM pid (to identify and kill our host `wsl.exe` on cancel) and runs apptainer as `"$@"`.
      // Every token here is simple (no spaces/quotes), so nothing is mangled by wsl.exe.
      val jvmPid = ProcessHandle.current().pid().toString
      Apptainer.backend.wrapApptainer(
        "bash",
        wrapperPath +: jvmPid +: Apptainer.apptainerPath +: cmd.args
      )
    else
      Apptainer.backend.wrapApptainer(Apptainer.apptainerPath, cmd.args)
  end execArgv
end DFToolsImage
