package dfhdl.internals
import java.net.JarURLConnection
import java.nio.file.{Files, Paths}
import scala.jdk.CollectionConverters.*

/** Helpers for working with bundled classpath resources of a foreign IP: enumerating every resource
  * under a package-like prefix (works both from a packaged jar and from an exploded build output),
  * and identifying the host platform subfolder to pick the right per-system simulator binaries.
  */
object ClasspathResources:

  private def classLoader: ClassLoader =
    Option(Thread.currentThread.getContextClassLoader).getOrElse(getClass.getClassLoader)

  /** List every (file) resource path under `prefix` (a `/`-separated classpath path, e.g.
    * `dfhdl/ips/video/vga/vga_monitor`). Returned paths are full classpath resource paths suitable
    * for `getResourceAsStream`. Handles both `jar:` URLs (entries enumerated from the JarFile) and
    * `file:` URLs (directory walked on disk). Returns an empty list if the prefix resolves to
    * nothing.
    */
  def listResources(prefix: String): List[String] =
    val norm = prefix.stripPrefix("/").stripSuffix("/")
    val results = scala.collection.mutable.LinkedHashSet.empty[String]
    val urls = classLoader.getResources(norm).asScala.toList
    urls.foreach { url =>
      url.getProtocol match
        case "jar" =>
          val conn = url.openConnection().asInstanceOf[JarURLConnection]
          val jar = conn.getJarFile
          jar.entries().asScala.foreach { e =>
            val name = e.getName
            if (!e.isDirectory && (name == norm || name.startsWith(norm + "/")))
              results += name
          }
        case "file" =>
          val base = Paths.get(url.toURI)
          if (Files.isDirectory(base))
            val stream = Files.walk(base)
            try
              stream.iterator().asScala.foreach { p =>
                if (Files.isRegularFile(p))
                  val rel = base.relativize(p).toString.replace('\\', '/')
                  results += s"$norm/$rel"
              }
            finally stream.close()
          else if (Files.isRegularFile(base)) results += norm
        case _ => // ignore other protocols
    }
    results.toList.sorted
  end listResources

  /** Read a classpath resource as raw bytes, or `None` if absent. */
  def readBytes(resourcePath: String): Option[Array[Byte]] =
    Option(classLoader.getResourceAsStream(resourcePath.stripPrefix("/"))).map { is =>
      try is.readAllBytes()
      finally is.close()
    }

  /** The host platform subfolder used to organize per-system IP binaries. These tags mirror the
    * vga-monitor release per-platform archive suffixes (`linux-x86_64`, `linux-arm64`,
    * `macos-x86_64`, `macos-arm64`, `windows-x86_64`, `windows-x86_64-mingw`); the IP build lays
    * the binaries out under subfolders named exactly these. On Windows two C runtimes coexist: MSVC
    * (for Questa) and MinGW (everything else), selected via [[windowsUsesMSVC]].
    */
  def hostPlatformTag(windowsUsesMSVC: Boolean = false): String =
    val arch = sys.props.getOrElse("os.arch", "").toLowerCase
    val isArm = arch.contains("aarch64") || arch.contains("arm64")
    if (osIsWindows) if (windowsUsesMSVC) "windows-x86_64" else "windows-x86_64-mingw"
    else if (osIsLinux) if (isArm) "linux-arm64" else "linux-x86_64"
    else if (isArm) "macos-arm64"
    else "macos-x86_64"
end ClasspathResources
