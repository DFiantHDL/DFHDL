package dfhdl.core
import dfhdl.compiler.ir
import java.nio.file.{Files, Path, Paths, StandardCopyOption}
import java.util.concurrent.ConcurrentHashMap
import scala.util.control.NonFatal

/** The sub-design cache service behind the elaboration design load gate. The cached artifact is a
  * plain hierarchical DB (an empty-members root whose `subDBs` hold the design's own sub-DB plus,
  * transitively, its child designs' sub-DBs, top first), serialized exactly like the top-design DB
  * cache: see `ir.DB.extractSubDesignDB` for extraction and `ir.DB.attachExternalSubDesigns` for
  * splicing into a loading run. The gate computes `localKey` (a digest of the intra-run cache key
  * parts: dclMeta, input DFTypes, Scala args, and the data-impure parameters' applied data) and
  * provides the def's owner class; the implementation completes the cross-run code identity (the
  * owner class's `factum.CodeRef` digest and the DFHDL version) and performs plain
  * content-addressed get/put.
  */
trait SubDesignCache:
  def lookup(ownerClass: Class[?], localKey: String): Option[ir.DB]
  def store(ownerClass: Class[?], localKey: String, cachedDB: ir.DB): Unit

/** The default disk-backed sub-design cache service (see `SubDesignCache`), enabled through
  * `ElaborationOptions.CacheEnable`. Each `MutableDB` (one per elaboration) instantiates its own
  * service (the `DesignLoadGate.subDesignCache` field is the per-elaboration injection seam for
  * tests), while the underlying stores are shared process-wide through thread-safe companion state.
  *
  * Entries live BESIDE the def's owner class build output (`<scala target dir>/dfhdl-cache/`), so a
  * build `clean` drops them together with the classes; content invalidation is carried by the key
  * itself: the owner class's `factum.CodeRef` digest (which also covers incremental recompilation,
  * where class files change without a clean), the DFHDL version, and the gate-computed `localKey`.
  * Top-level design defs are covered like any other: Scala places them in the synthetic
  * `<file>$package` class, whose class file lives in the same build output. Owner classes with no
  * writable directory code source (e.g. defs shipped inside library jars) skip the disk tier
  * (miss-safe; the in-memory store and the intra-run tier still cover them).
  */
final class SubDesignDiskCache extends SubDesignCache:
  import SubDesignDiskCache.*
  def lookup(ownerClass: Class[?], localKey: String): Option[ir.DB] =
    fullKeyOf(ownerClass, localKey).flatMap { fullKey =>
      Option(memStore.get(fullKey)).orElse {
        entryFileOf(ownerClass, fullKey).filter(Files.exists(_)).flatMap { file =>
          // a corrupt or unreadable entry is just a miss
          try
            val db = ir.DB.fromJsonString(Files.readString(file))
            memStore.put(fullKey, db)
            Some(db)
          catch case NonFatal(_) => None
        }
      }
    }
  def store(ownerClass: Class[?], localKey: String, cachedDB: ir.DB): Unit =
    fullKeyOf(ownerClass, localKey).foreach { fullKey =>
      memStore.put(fullKey, cachedDB)
      entryFileOf(ownerClass, fullKey).foreach { file =>
        try
          Files.createDirectories(file.getParent)
          // temp-file + atomic move keeps concurrent writers (parallel test forks)
          // consistent; both write the same content for the same key anyway
          val tmp = Files.createTempFile(file.getParent, file.getFileName.toString, ".tmp")
          Files.writeString(tmp, cachedDB.toJsonString)
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
  // AggregateStore layer bytes, but the expensive part here is the JSON parse, so this
  // memoizes the DESERIALIZED DB, keyed by the full content key): repeated elaborations
  // in one JVM session (e.g. an sbt server) skip both the file read and the parsing, a
  // run's write-back serves later runs from memory, and defs with no writable disk
  // location (jar-shipped) still cache in-process. Cached DBs are immutable and
  // attachment freshens tokens per run, so sharing the instances is safe.
  private val memStore = new ConcurrentHashMap[String, ir.DB]
  // testing/diagnostic: drops the process-wide in-memory store (disk entries remain)
  def clearInMemoryStore(): Unit = memStore.clear()

  private def codeDigestOf(cls: Class[?]): Option[String] =
    codeDigestMemo.computeIfAbsent(
      cls,
      cls =>
        try Some(factum.CodeRef(cls).digest.asString)
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
  // the full content key: owner class code digest + DFHDL version + the gate's
  // localKey (None when the code digest is unattainable, making the call disk-less)
  private def fullKeyOf(cls: Class[?], localKey: String): Option[String] =
    codeDigestOf(cls).map(digest => s"$digest|${dfhdl.dfhdlVersion}|$localKey")
  private def entryFileOf(cls: Class[?], fullKey: String): Option[Path] =
    cacheDirOf(cls).map { dir =>
      val hex = java.security.MessageDigest.getInstance("SHA-256")
        .digest(fullKey.getBytes("UTF-8"))
        .map("%02x".format(_)).mkString
      dir.resolve(s"$hex.dfdb.json")
    }
end SubDesignDiskCache
