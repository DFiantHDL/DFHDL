package dfhdl.internals

import java.net.{URI, URL}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import java.security.MessageDigest
import java.util.concurrent.ConcurrentHashMap
import scala.collection.mutable
import scala.util.control.NonFatal

/** ~~~ the runtime half of the elaboration code digest ~~~
  *
  * The code identity of a class: a digest that changes whenever the class's own code, or the code
  * of anything it reaches, changes. The sub-design disk cache and the app-level elaboration cache
  * key their entries by it, so an entry outlives exactly the runs whose code produced it.
  *
  * The compiler plugin's `CodeDigest` phase wrote, beside each class it compiled, a record of that
  * class's OWN code hash and the top-level classes it REACHES (see the phase for the format). This
  * composes the digest by folding those records over the transitive closure. Composition happens
  * HERE, at runtime, and not at compile time: a helper's rebuild does not force its dependents to
  * recompile (zinc keeps their class files), so only a fold over the CURRENT records sees it.
  *
  * The closure stops at two boundaries, which is what keeps it cheap. DFHDL itself is digested as
  * the library VERSION and never scanned: that is what the library is to every user (a released
  * jar), and a DFHDL development build gets the same treatment deliberately, so the boundary is one
  * rule instead of two. And a JAR is digested as that jar, a versioned artifact whose code cannot
  * reach the build output anyway.
  *
  * The one jar that is NOT such an artifact is the one holding the design itself. Under sbt a
  * `runMain` runs off a jar repackaged from the build output, under a fresh `bg-jobs/job-N/` path,
  * on every single run: the user's own code is a class IN a jar there. That jar is the code under
  * development, so the scan reads records INSIDE it (they are packaged with the classes) rather
  * than folding it whole -- which would key every design on a throwaway artifact and retire the
  * cache once a run. Every OTHER jar is a dependency, and the scan stops at it.
  *
  * Blind spots, all shared with any static approach: reflection and dynamic dispatch are invisible,
  * and a class compiled WITHOUT the plugin names no dependencies of its own. A class with no record
  * at all yields no digest (`of` returns `None`), which callers must read as "not cacheable" rather
  * than "unchanged".
  */
object CodeDigest:
  // the record format, shared by hand with the writer (the plugin's `CodeDigest` phase)
  private final val recordExt = ".dfdigest"
  private final val classExt = ".class"
  private final val formatHeader = "dfhdl-digest 1"
  // the DFHDL library's own package root (see the library boundary in `identityOf`)
  private final val libraryPackagePrefix = "dfhdl."

  /** How a class contributes to a digest. */
  private enum Identity derives CanEqual:
    /** a plugin-compiled class: its own code hash, and the classes it reaches */
    case Record(own: String, deps: List[String])

    /** a class digested whole, naming no dependencies of its own: the DFHDL library (as its
      * version), the jar that ships it (as that jar's identity), or its class file (as its stamp)
      */
    case Opaque(token: String)

    /** no class file behind the name (a name reached only through code that is gone) */
    case Missing
  import Identity.*

  // process-wide memos. Keyed by class name, which assumes one classpath per process: a name that
  // resolves through two different class loaders to two different classes would collide (an
  // application server, not a build tool or a DFHDL app).
  private val identityMemo = new ConcurrentHashMap[String, Identity]
  private val digestMemo = new ConcurrentHashMap[String, String]
  private val locationMemo = new ConcurrentHashMap[String, String]

  /** testing/diagnostic: forget every stamp taken so far */
  def clearMemos(): Unit =
    identityMemo.clear()
    digestMemo.clear()
    locationMemo.clear()

  private def sha256(str: String): String = hex(
    MessageDigest.getInstance("SHA-256").digest(str.getBytes(StandardCharsets.UTF_8))
  )
  private def hex(bytes: Array[Byte]): String = bytes.map("%02x".format(_)).mkString

  /** The code identity of `cls`, or `None` when the plugin never compiled it (no record, so its
    * dependencies are unknown and a digest would be a promise the fold cannot keep).
    *
    * `libraryVersion` is what DFHDL itself digests to, the fold stopping at the library boundary.
    */
  def of(cls: Class[?], libraryVersion: String): Option[String] =
    // records are written per TOP-LEVEL class, whose code (and reference closure) covers every
    // class nested in it: a design's declaring class is often a nested, local or anonymous one
    var top = cls
    while (top.getEnclosingClass != null) top = top.getEnclosingClass
    val name = top.getName
    val loader = Option(cls.getClassLoader).getOrElse(ClassLoader.getSystemClassLoader)
    // the design's own jar, if it is in one, is THE development jar of this run (see the class
    // doc): the scan reads records inside it, and stops at every other jar
    val scope = Scope(loader, libraryVersion, devJar = jarOf(name, loader))
    // the ANCHOR is read as a record even when it is library code (a design shipped in DFHDL's own
    // lib is cacheable like any other); the library boundary applies to what it REACHES
    recordOf(name, loader).map { record =>
      digestMemo.computeIfAbsent(
        s"$name@${scope.devJar.getOrElse("")}",
        _ => compose(name, record, scope)
      )
    }
  end of

  /** What the fold resolves names against: the class loader, the DFHDL version the library folds
    * to, and the run's development jar (the jar the design itself came from, if any).
    */
  private final case class Scope(
      loader: ClassLoader,
      libraryVersion: String,
      devJar: Option[String]
  )

  /** The digest of `anchor`: a hash over its whole reachable closure, each class contributing its
    * own code hash, its bytes, or the library version. Sorted by name, so the fold does not depend
    * on the traversal order.
    */
  private def compose(anchor: String, anchorRecord: Identity, scope: Scope): String =
    val parts = mutable.TreeMap.empty[String, String]
    val pending = mutable.Stack.empty[String]
    def add(name: String, identity: Identity): Unit = identity match
      case Record(own, deps) =>
        parts += name -> own
        // cycles are cut by `parts` alone: a class already stamped is never re-entered
        deps.foreach(dep => if (!parts.contains(dep)) pending.push(dep))
      case Opaque(token) => parts += name -> token
      case Missing       => parts += name -> "<missing>"
    add(anchor, anchorRecord)
    while (pending.nonEmpty)
      val name = pending.pop()
      if (!parts.contains(name)) add(name, identityOf(name, scope))
    sha256(parts.iterator.map((name, part) => s"$name=$part").mkString("\n"))
  end compose

  private def identityOf(name: String, scope: Scope): Identity =
    identityMemo.computeIfAbsent(
      s"$name@${scope.devJar.getOrElse("")}",
      _ =>
        // THE LIBRARY BOUNDARY: DFHDL is digested as its version and never scanned, exactly as the
        // released jar a real user depends on would be
        if (name.startsWith(libraryPackagePrefix)) Opaque(s"dfhdl@${scope.libraryVersion}")
        else
          jarOf(name, scope.loader) match
            // A DEPENDENCY JAR: a versioned artifact, digested whole. Its code cannot reach the
            // build output, so nothing inside it is worth scanning.
            case Some(jar) if !scope.devJar.contains(jar) =>
              Opaque(locationMemo.computeIfAbsent(jar, _ => stampOf(Paths.get(URI(jar)))))
            // the build output: this run's development jar, or a plain class directory. Code under
            // development, so its record (what it is and what it reaches) is what counts.
            case _ =>
              recordOf(name, scope.loader).getOrElse(classFileStamp(name, scope.loader))
    )

  /** The class's own build output, for a class the plugin never compiled: its class file's stamp
    * (it names no dependencies of its own).
    */
  private def classFileStamp(name: String, loader: ClassLoader): Identity =
    try
      loader.getResource(s"${name.replace('.', '/')}$classExt") match
        case null                             => Missing
        case url if url.getProtocol == "file" => Opaque(stampOf(Paths.get(url.toURI)))
        // inside the development jar: the entry's own bytes, since the jar itself is rebuilt (and
        // renamed) per run and says nothing about the code
        case url => Opaque(sha256(url.toString) + ":" + entryHash(url))
    catch case NonFatal(_) => Missing

  /** The content hash of a classpath resource (a class file inside the development jar). */
  private def entryHash(url: URL): String =
    try
      val in = url.openStream()
      try hex(MessageDigest.getInstance("SHA-256").digest(in.readAllBytes()))
      finally in.close()
    catch case NonFatal(_) => "<unhashed>"

  /** The jar a class is loaded from, if any. */
  private def jarOf(name: String, loader: ClassLoader): Option[String] =
    try
      Option(loader.getResource(s"${name.replace('.', '/')}$classExt"))
        .filter(_.getProtocol == "jar")
        .map(_.getPath.takeWhile(_ != '!'))
    catch case NonFatal(_) => None

  private def stampOf(file: Path): String =
    try s"${Files.size(file)}:${Files.getLastModifiedTime(file).toMillis}"
    catch case NonFatal(_) => "<unstamped>"

  /** The record the plugin wrote for this class, from the SAME classpath entry as the class file
    * (both sit at the same package path under their root, so the two URLs must agree everywhere but
    * the extension). Works in a jar as well as a directory: the records are packaged with the
    * classes.
    */
  private def recordOf(name: String, loader: ClassLoader): Option[Identity] =
    try
      val path = name.replace('.', '/')
      for
        classUrl <- Option(loader.getResource(s"$path$classExt"))
        recordUrl <- Option(loader.getResource(s"$path$recordExt"))
        if recordUrl.toString.stripSuffix(recordExt) == classUrl.toString.stripSuffix(classExt)
        record <- readRecord(recordUrl)
      yield record
    catch case NonFatal(_) => None

  private def readRecord(url: URL): Option[Identity] =
    try
      val in = url.openStream()
      val text =
        try String(in.readAllBytes(), StandardCharsets.UTF_8)
        finally in.close()
      val lines = text.linesIterator.toList
      lines.headOption.filter(_ == formatHeader).flatMap { _ =>
        val own = lines.collectFirst { case l if l.startsWith("own ") => l.stripPrefix("own ") }
        val deps = lines.collect { case l if l.startsWith("dep ") => l.stripPrefix("dep ") }
        own.map(Record(_, deps))
      }
    // a truncated or half-written record is treated as absent, never as a match
    catch case NonFatal(_) => None

end CodeDigest
