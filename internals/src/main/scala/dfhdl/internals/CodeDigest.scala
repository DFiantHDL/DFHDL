package dfhdl.internals

import java.net.URL
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import java.security.MessageDigest
import java.util.concurrent.ConcurrentHashMap
import scala.collection.mutable
import scala.util.control.NonFatal

/** ~~~ the runtime half of the elaboration code digest ~~~
  *
  * The code identity of a class: a digest that changes whenever the class's own code, or the code
  * of anything it reaches, changes. The sub-design disk cache keys its entries by it, so an entry
  * outlives exactly the runs whose code produced it.
  *
  * The compiler plugin's `CodeDigest` phase wrote, beside each class it compiled, a record of that
  * class's OWN code hash and the top-level classes it REACHES (see the phase for the format). This
  * composes the digest by folding those records over the transitive closure. Composition happens
  * HERE, at runtime, and not at compile time: a helper's rebuild does not force its dependents to
  * recompile (zinc keeps their class files), so only a fold over the CURRENT records sees it.
  *
  * Two boundaries keep the closure small enough to be free. A class in a JAR needs no closure at
  * all: jar code cannot reach build output, so the jar's own identity (path, size, timestamp)
  * covers that class and everything it reaches inside. And DFHDL itself is digested as its VERSION
  * and never scanned, which is what it looks like to every user (a released jar) and, deliberately,
  * to a DFHDL development build too. What remains is the code the user is actually editing.
  *
  * Blind spots, all shared with any static approach: reflection and dynamic dispatch are invisible,
  * and a class compiled WITHOUT the plugin (no record, no dependencies) contributes its class file's
  * stamp alone. A class with no record at all yields no digest (`of` returns `None`), which callers
  * must read as "not cacheable" rather than "unchanged".
  */
object CodeDigest:
  // the record format, shared by hand with the writer (the plugin's `CodeDigest` phase)
  private final val recordExt = ".dfdigest"
  private final val formatHeader = "dfhdl-digest 1"
  // the DFHDL library's own package root (see the library boundary in `fileIdentity`)
  private final val libraryPackagePrefix = "dfhdl."

  /** The DFHDL library's identity: its version, which is what a released jar is known by. */
  private final case class LibraryTag(version: String)

  /** How a class contributes to a digest. */
  private enum Identity derives CanEqual:
    /** a plugin-compiled class in the build output: its own code hash, and what it reaches */
    case Record(own: String, deps: List[String])

    /** a class digested as a whole: a jar (`fromJar`, covering its contents), or a class file with
      * no record of its own (which therefore contributes no dependencies)
      */
    case Coarse(token: String, fromJar: Boolean)

    /** no class file behind the name (a name reached only through code that is gone) */
    case Missing
  import Identity.*

  // process-wide memos. Keyed by class name, which assumes one classpath per process: a name that
  // resolves through two different class loaders to two different classes would collide (an
  // application server, not a build tool or a DFHDL app).
  private val identityMemo = new ConcurrentHashMap[String, Identity]
  private val digestMemo = new ConcurrentHashMap[String, String]
  private val locationMemo = new ConcurrentHashMap[String, String]

  /** testing/diagnostic: forget every stamp taken so far (a rebuild inside a live JVM, which a
    * build tool's forked run never sees, would otherwise keep answering from the memos)
    */
  def clearMemos(): Unit =
    identityMemo.clear()
    digestMemo.clear()
    locationMemo.clear()

  private def sha256(str: String): String =
    MessageDigest.getInstance("SHA-256")
      .digest(str.getBytes(StandardCharsets.UTF_8))
      .map("%02x".format(_)).mkString

  /** The code identity of `cls`, or `None` when it has none: an unplaceable class, or one compiled
    * without the plugin (no record, so its dependencies are unknown and a digest would be a promise
    * the fold cannot keep).
    *
    * `libraryVersion` identifies the DFHDL library itself, and the fold stops there: a class of the
    * library (no record of its own, since the library's own modules are not plugin-compiled) is
    * digested as that version and is not scanned further. This is what the library looks like to
    * every real user, who depends on a released jar; a DFHDL DEVELOPMENT build gets the same
    * treatment rather than a class-file walk of the whole library, so the boundary is one rule
    * instead of two.
    */
  def of(cls: Class[?], libraryVersion: String): Option[String] =
    // records are written per TOP-LEVEL class, whose code (and reference closure) covers every
    // class nested in it: a design's declaring class is often a nested, local or anonymous one
    var top = cls
    while (top.getEnclosingClass != null) top = top.getEnclosingClass
    val name = top.getName
    val loader = Option(cls.getClassLoader).getOrElse(ClassLoader.getSystemClassLoader)
    val lib = LibraryTag(libraryVersion)
    identityOf(name, loader, lib) match
      case Record(_, _) => Some(digestMemo.computeIfAbsent(name, _ => compose(name, loader, lib)))
      // a jar IS the closure of the classes it holds (jar code cannot reach build output)
      case Coarse(token, true) => Some(sha256(s"$name=$token"))
      case _                   => None

  /** The digest of `anchor`: a hash over its whole reachable closure, each class contributing its
    * own code hash (a plugin-compiled class), the identity of the jar holding it, or its class
    * file's stamp. Sorted by name, so the fold does not depend on the traversal order.
    */
  private def compose(anchor: String, loader: ClassLoader, lib: LibraryTag): String =
    val parts = mutable.TreeMap.empty[String, String]
    val pending = mutable.Stack(anchor)
    while (pending.nonEmpty)
      val name = pending.pop()
      if (!parts.contains(name))
        identityOf(name, loader, lib) match
          case Record(own, deps) =>
            parts += name -> own
            // cycles are cut by `parts` alone: a class already stamped is never re-entered
            deps.foreach(dep => if (!parts.contains(dep)) pending.push(dep))
          case Coarse(token, _) => parts += name -> token
          case Missing          => parts += name -> "<missing>"
    sha256(parts.iterator.map((name, part) => s"$name=$part").mkString("\n"))
  end compose

  private def identityOf(name: String, loader: ClassLoader, lib: LibraryTag): Identity =
    identityMemo.computeIfAbsent(name, _ => locate(name, loader, lib))

  private def locate(name: String, loader: ClassLoader, lib: LibraryTag): Identity =
    try
      val path = name.replace('.', '/')
      loader.getResource(s"$path.class") match
        case null => Missing
        case url  =>
          url.getProtocol match
            case "jar"  => Coarse(jarToken(url), fromJar = true)
            case "file" => fileIdentity(Paths.get(url.toURI), name, path, loader, lib)
            case _      => Missing
    catch case NonFatal(_) => Missing

  private def fileIdentity(
      classFile: Path,
      name: String,
      path: String,
      loader: ClassLoader,
      lib: LibraryTag
  ): Identity =
    val record = Option(loader.getResource(s"$path$recordExt")).filter(_.getProtocol == "file")
      .map(url => Paths.get(url.toURI))
      // the record must come from the SAME classpath entry as the class file: sharing a package
      // directory is exactly that, since both are placed at the same package path under their root
      .filter(_.getParent.toString == classFile.getParent.toString)
      .flatMap(readRecord)
    record.getOrElse {
      // THE LIBRARY BOUNDARY: a DFHDL class with no record of its own is library code compiled
      // without the plugin (the language core, the IR, these internals). It is digested as the
      // library version and never scanned, exactly as it would be if it came from the jar a real
      // user depends on. Everything else with no record (Java code, a module built without the
      // plugin) still contributes its class file's stamp.
      if (name.startsWith(libraryPackagePrefix)) Coarse(s"dfhdl@${lib.version}", fromJar = false)
      else Coarse(stampOf(classFile), fromJar = false)
    }
  end fileIdentity

  private def readRecord(file: Path): Option[Identity] =
    try
      val lines = Files.readString(file, StandardCharsets.UTF_8).linesIterator.toList
      lines.headOption.filter(_ == formatHeader).flatMap { _ =>
        val own = lines.collectFirst { case l if l.startsWith("own ") => l.stripPrefix("own ") }
        val deps = lines.collect { case l if l.startsWith("dep ") => l.stripPrefix("dep ") }
        own.map(Record(_, deps))
      }
    // a truncated or half-written record is treated as absent, never as a match
    catch case NonFatal(_) => None

  /** The identity of a jar behind a `jar:file:/...!/pkg/Cls.class` URL: taken once per jar, since a
    * jar is a whole build artifact and every class in it shares its fate.
    */
  private def jarToken(url: URL): String =
    val jarPath = url.getPath.takeWhile(_ != '!')
    locationMemo.computeIfAbsent(
      jarPath,
      _ =>
        try stampOf(Paths.get(java.net.URI(jarPath)))
        catch case NonFatal(_) => "<unstamped>"
    )

  private def stampOf(file: Path): String =
    try s"${Files.size(file)}:${Files.getLastModifiedTime(file).toMillis}"
    catch case NonFatal(_) => "<unstamped>"
end CodeDigest
