package dfhdl.core
import dfhdl.compiler.ir
import java.nio.file.{Files, Path, Paths, StandardCopyOption}
import java.util.concurrent.ConcurrentHashMap
import scala.util.control.NonFatal

/** The sub-design cache service behind the elaboration design load gate. The cached artifact is an
  * `ir.SubDesignEntry`: ONE design's own sub-DB plus its child designs BY CACHE KEY (the loading
  * run resolves each child through the gate, like a live instantiation). The gate computes
  * `localKey` (a digest of the intra-run cache key parts: dclMeta, input DFTypes, Scala args, and
  * the data-impure parameters' applied data) and provides the def's owner class; the implementation
  * completes the cross-run code identity (the owner class's code digest and the DFHDL version) and
  * performs plain content-addressed get/put.
  */
trait SubDesignCache:
  def lookup(ownerClass: Class[?], localKey: String): Option[ir.SubDesignEntry]
  def store(ownerClass: Class[?], localKey: String, entry: ir.SubDesignEntry): Unit

/** The default disk-backed sub-design cache service (see `SubDesignCache`), enabled through
  * `ElaborationOptions.CacheEnable`. Each `MutableDB` (one per elaboration) instantiates its own
  * service (the `DesignLoadGate.subDesignCache` field is the per-elaboration injection seam for
  * tests), while the underlying stores are shared process-wide through thread-safe companion state.
  *
  * Entries live BESIDE the def's owner class build output (`<scala target dir>/dfhdl-cache/`), so a
  * build `clean` drops them together with the classes; content invalidation is carried by the key
  * itself: the owner class's code digest (`dfhdl.internals.CodeDigest`, which also covers
  * incremental recompilation, where class files change without a clean), the DFHDL version, and the
  * gate-computed `localKey`. Top-level methods are covered like any other: Scala places them in the
  * synthetic `<file>$package` class, whose class file lives in the same build output. Owner classes
  * with no writable directory code source (e.g. defs shipped inside library jars) skip the disk
  * tier (miss-safe; the in-memory store and the intra-run tier still cover them).
  */
final class SubDesignDiskCache extends SubDesignCache:
  import SubDesignDiskCache.*
  def lookup(ownerClass: Class[?], localKey: String): Option[ir.SubDesignEntry] =
    fullKeyOf(ownerClass, localKey).flatMap { fullKey =>
      Option(memStore.get(fullKey)).orElse {
        entryFileOf(ownerClass, fullKey).filter(Files.exists(_)).flatMap { file =>
          try Some(Files.readString(file))
          catch case NonFatal(_) => None
        }
      }.flatMap { json =>
        // deserialized per hit, so every elaboration adopts its OWN member objects (IR
        // members carry per-run mutable caches; sharing them across runs is unsafe)
        try
          val entry = ir.SubDesignEntry.fromJsonString(json)
          memStore.put(fullKey, json)
          Some(entry)
        // a corrupt entry is just a miss (and never enters the memory store)
        catch case NonFatal(_) => None
      }
    }
  def store(ownerClass: Class[?], localKey: String, entry: ir.SubDesignEntry): Unit =
    fullKeyOf(ownerClass, localKey).foreach { fullKey =>
      val json = entry.toJsonString
      memStore.put(fullKey, json)
      entryFileOf(ownerClass, fullKey).foreach { file =>
        try
          Files.createDirectories(file.getParent)
          // temp-file + atomic move keeps concurrent writers (parallel test forks)
          // consistent; both write the same content for the same key anyway
          val tmp = Files.createTempFile(file.getParent, file.getFileName.toString, ".tmp")
          Files.writeString(tmp, json)
          try Files.move(tmp, file, StandardCopyOption.ATOMIC_MOVE)
          catch
            case NonFatal(_) =>
              Files.move(tmp, file, StandardCopyOption.REPLACE_EXISTING)
        catch
          case NonFatal(_) => // failing to store is not an error; the run stays live
      }
    }
end SubDesignDiskCache

object SubDesignDiskCache:
  // ~~~ process-wide, thread-safe shared state ~~~
  // per-process memos: CodeRef re-reads the owner class's whole reference closure
  private val codeDigestMemo = new ConcurrentHashMap[Class[?], Option[String]]
  private val cacheDirMemo = new ConcurrentHashMap[Class[?], Option[Path]]
  // The process-wide in-memory tier in front of the disk (factum's MemoryStore/
  // AggregateStore layer), holding the serialized entry keyed by the full content key:
  // repeated elaborations in one JVM session (e.g. an sbt server) skip the file read, a
  // run's write-back serves later runs from memory, and defs with no writable disk
  // location (jar-shipped) still cache in-process. It memoizes the JSON and NOT the
  // deserialized DB: IR members carry per-run mutable caches (`HasRefCompare`'s compare
  // memo, a design block's elaboration-time instance cache), so two elaborations must
  // never adopt the same member objects.
  private val memStore = new ConcurrentHashMap[String, String]
  // testing/diagnostic: drops the process-wide in-memory store (disk entries remain)
  def clearInMemoryStore(): Unit = memStore.clear()

  private def codeDigestOf(cls: Class[?]): Option[String] =
    codeDigestMemo.computeIfAbsent(
      cls,
      cls =>
        try dfhdl.internals.CodeDigest.of(cls, dfhdl.dfhdlVersion)
        catch case NonFatal(_) => None
    )
  private def cacheDirOf(cls: Class[?]): Option[Path] =
    cacheDirMemo.computeIfAbsent(
      cls,
      cls =>
        try
          val location = Paths.get(cls.getProtectionDomain.getCodeSource.getLocation.toURI)
          // only directory classpath entries (build output) get an adjacent cache
          // folder; jar-located classes skip the disk tier
          if (Files.isDirectory(location)) Some(location.resolveSibling("dfhdl-cache"))
          else None
        catch case NonFatal(_) => None
    )
  // the full content key: the owner class's code digest (which folds in the DFHDL version, the
  // library being the boundary of the digest's scan) and the gate's localKey (None when the code
  // digest is unattainable, making the call disk-less)
  private def fullKeyOf(cls: Class[?], localKey: String): Option[String] =
    codeDigestOf(cls).map(digest => s"$digest|$localKey")
  private def entryFileOf(cls: Class[?], fullKey: String): Option[Path] =
    cacheDirOf(cls).map { dir =>
      val hex = java.security.MessageDigest.getInstance("SHA-256")
        .digest(fullKey.getBytes("UTF-8"))
        .map("%02x".format(_)).mkString
      dir.resolve(s"$hex.dfdb.json")
    }
end SubDesignDiskCache
