package dfhdl.tools.toolsCore
import scalapptainer.*

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

  /** Map an in-image executable name to its DFTools image. Yosys depends on the backend dialect
    * (VHDL synthesis loads the ghdl frontend, hence `synth-vhdl`).
    */
  def imageFor(exec: String, vhdl: Boolean): String = exec match
    case "yosys" => if (vhdl) "synth-vhdl" else "synth-verilog"
    case "eqy"   => "synth-verilog"
    case "nextpnr-ecp5" | "nextpnr-himbaechel" | "ecppack" | "gowin_pack" => "pnr"
    case "ghdl" | "nvc"                                                   => "sim-llvm"
    case "verilator" | "verilator_bin"                                    => "sim-verilator"
    case "iverilog" | "vvp"                                               => "sim-iverilog"
    case "surfer"                                                         => "wavegen"
    case "openFPGALoader"                                                 => "program"
    case other => throw new IllegalArgumentException(s"no DFTools image for tool '$other'")

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

  /** Build the host argv for `apptainer exec [opts] <image> <containerCmd...>`, optionally
    * forwarding X11 (for GUI tools such as the waveform viewer).
    */
  def execArgv(image: String, containerCmd: Seq[String], withX11: Boolean): Seq[String] =
    val img = if (withX11) handle(image).withX11() else handle(image)
    val cmd = commands.ExecCommand(img.ref, containerCmd, img.options)
    Apptainer.backend.wrapApptainer(Apptainer.apptainerPath, cmd.args)
end DFToolsImage
