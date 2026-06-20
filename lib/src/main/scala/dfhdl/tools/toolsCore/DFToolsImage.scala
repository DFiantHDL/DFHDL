package dfhdl.tools.toolsCore
import scalapptainer.*

/** Resolves and caches the DFTools Apptainer image that DFHDL runs the external EDA tools from when
  * `tools-location` is `dftools` (the default).
  *
  * Resolution order:
  *   1. the dev/test override system property `dfhdl.dftools.sif` (a backend-accessible .sif path)
  *      — used by DFTools CI to validate a freshly-built image before it is published;
  *   2. otherwise the published release asset for the current architecture, downloaded into
  *      Scalapptainer's image cache once and reused.
  *
  * The actual `apptainer exec` invocation is built and spawned by [[Tool.exec]] (so it shares
  * DFHDL's stdout/cancellation handling); this object only resolves the image handle.
  */
object DFToolsImage:
  /** The DFTools release this DFHDL build targets. Bump when adopting a new DFTools release. */
  val version: String = "v0.1.0"
  private val repo = "DFiantHDL/DFTools"

  /** Dev/test override: run against a specific local .sif (a path valid inside the backend). */
  def overrideSif: Option[String] =
    Option(System.getProperty("dfhdl.dftools.sif")).map(_.trim).filter(_.nonEmpty)

  /** `linux-x64` | `linux-arm64`, per the backend's reported machine type. */
  private lazy val archTag: String =
    Apptainer.backend.runShell("uname -m").out.trim match
      case "aarch64" | "arm64" => "linux-arm64"
      case _                   => "linux-x64"

  private def assetUrl: String =
    val v = version.stripPrefix("v")
    s"https://github.com/$repo/releases/download/$version/dftools-$v-$archTag.sif"

  /** The resolved image handle (memoized). Downloads the release asset on first use if needed. */
  lazy val image: ApptainerImage =
    overrideSif match
      case Some(p) => Apptainer.image(p)
      case None    =>
        val dest = s"${Apptainer.imagesDir}/dftools-$version-$archTag.sif"
        val img = Apptainer.image(dest)
        if (!img.exists)
          Apptainer.backend
            .runShell(
              s"mkdir -p ${ShellQuote.single(Apptainer.imagesDir)} && " +
                s"curl -fL --retry 3 -o ${ShellQuote.single(dest)} ${ShellQuote.single(assetUrl)}"
            )
            .throwIfFailed()
        Apptainer.image(dest)

  /** Whether the DFTools image is resolvable (present locally / overridden). */
  def isAvailable: Boolean =
    try image.exists
    catch case _: Throwable => false

  /** The resolved apptainer binary path inside the backend (installing in user mode on first call).
    */
  def apptainerPath: String = Apptainer.apptainerPath

  /** Build the host argv for `apptainer exec [opts] <image> <containerCmd...>`, optionally
    * forwarding X11 (for GUI tools such as the waveform viewer). `containerCmd` is the bare tool
    * argv as seen inside the Linux image.
    */
  def execArgv(containerCmd: Seq[String], withX11: Boolean): Seq[String] =
    val img = if (withX11) image.withX11() else image
    val cmd = commands.ExecCommand(img.ref, containerCmd, img.options)
    Apptainer.backend.wrapApptainer(apptainerPath, cmd.args)

  /** Run a version probe inside the image and return its raw output (empty on failure). */
  def probe(containerCmd: Seq[String]): String =
    try image.exec(containerCmd*).out
    catch case _: Throwable => ""
end DFToolsImage
