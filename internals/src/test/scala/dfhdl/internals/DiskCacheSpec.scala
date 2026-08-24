package dfhdl.internals

import munit.FunSuite
import java.nio.file.{Files, Path}
import java.util.concurrent.atomic.AtomicInteger

class DiskCacheSpec extends FunSuite:
  private def deleteRecursively(p: Path): Unit =
    if (Files.isDirectory(p))
      val stream = Files.list(p)
      try stream.forEach(deleteRecursively)
      finally stream.close()
    Files.deleteIfExists(p)

  // The hit-validation seam of the DFApp elaborate step: a step whose cached value records
  // external state (init files an elaboration read) validates it on every hit, and a rejected
  // value re-runs the step and overwrites the entry under the same key.
  test("a rejected cache-hit validation re-runs the step and overwrites the entry") {
    val cacheDir = Files.createTempDirectory("dfhdl-diskcache-spec")
    try
      val runs = AtomicInteger(0)
      val invalidations = AtomicInteger(0)
      var current = "v1"
      var accept = true
      object cache extends DiskCache(cacheDir.toString)
      object step extends cache.Step[Unit, String](() => ())():
        protected def run(from: Unit): String =
          runs.incrementAndGet()
          current
        protected def valueToCacheStr(value: String): String = value
        protected def cacheStrToValue(str: String): String = str
        override protected def cacheHitValidator: Option[String => Boolean] =
          Some(_ => accept)
        override protected def logCacheInvalidated(): Unit =
          invalidations.incrementAndGet()
      end step
      assertEquals(step(), "v1")
      assertEquals(step(), "v1") // accepted hit
      assertEquals(runs.get, 1)
      assertEquals(invalidations.get, 0)
      accept = false
      current = "v2"
      assertEquals(step(), "v2") // rejected: the step re-runs like a miss
      assertEquals(runs.get, 2)
      assertEquals(invalidations.get, 1)
      accept = true
      assertEquals(step(), "v2") // the re-run overwrote the entry under the same key
      assertEquals(runs.get, 2)
    finally deleteRecursively(cacheDir)
    end try
  }
end DiskCacheSpec
