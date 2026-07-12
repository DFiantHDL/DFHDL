package StagesSpec

import dfhdl.*
import dfhdl.compiler.ir
import dfhdl.compiler.stages.dropDesignDefs
import dfhdl.core.{DFC, SubDesignCache, SubDesignDiskCache}
// scalfmt: { align.tokens = [{code = "<>"}, {code = "="}, {code = "=>"}, {code = ":="}]}

// a TOP-LEVEL design def: its owner class is the synthetic `<file>$package` class, so
// the disk cache entry lives beside this file's build output like any other class
def topCalc(arg: UInt[8] <> VAL): UInt[8] <> DFRET =
  arg + 1

/** Tests for the sub-design cache tier of the elaboration design load gate
  * (`ElaborationOptions.CacheEnable`): a pure design def whose cached DB is found by the
  * `SubDesignDiskCache` service skips its body elaboration entirely; the harness still creates the
  * public interface (ports and parameters, bound fresh to the call's applied values) and the cached
  * hierarchical DB is attached to the final DB. A fabricated hit must elaborate to the exact same
  * code as a live run.
  */
class SubDesignCacheSpec extends StageSpec(stageCreatesUnrefAnons = true):
  // a map-backed cache service that round-trips cached DBs through their JSON
  // serialization (exercising the same path the disk service uses)
  class MapSubDesignCache extends SubDesignCache:
    val entries = collection.mutable.Map.empty[(Class[?], String), String]
    var hits = 0
    def lookup(ownerClass: Class[?], localKey: String): Option[ir.DB] =
      val res = entries.get((ownerClass, localKey)).map(ir.DB.fromJsonString(_))
      if (res.nonEmpty) hits += 1
      res
    def store(ownerClass: Class[?], localKey: String, cachedDB: ir.DB): Unit =
      entries((ownerClass, localKey)) = cachedDB.toJsonString
  end MapSubDesignCache

  // wraps the real disk service with hit/store counters
  class CountingSubDesignCache(underlying: SubDesignCache) extends SubDesignCache:
    var hits = 0
    var stores = 0
    def lookup(ownerClass: Class[?], localKey: String): Option[ir.DB] =
      val res = underlying.lookup(ownerClass, localKey)
      if (res.nonEmpty) hits += 1
      res
    def store(ownerClass: Class[?], localKey: String, cachedDB: ir.DB): Unit =
      stores += 1
      underlying.store(ownerClass, localKey, cachedDB)
  end CountingSubDesignCache

  // the def captures the host's `w` (a phantom design parameter), so the cached DB also
  // round-trips phantom members. Elaboration options do not flow into a plain `new`
  // instantiation (no @top annotation), so the host is generated under an explicit DFC
  // carrying the test's options (fresh per elaboration).
  def genHost(using DFC): dfhdl.core.Design =
    class Host extends DFDesign:
      val data = UInt(8) <> IN
      val w: UInt[8] <> CONST = 7
      val o = UInt(8) <> OUT
      val t = UInt(8) <> OUT
      def calc(arg: UInt[8] <> VAL): UInt[8] <> DFRET =
        arg + w
      o := calc(data)
      t := topCalc(data)
    end Host
    new Host
  end genHost

  // each elaboration gets a fresh DFC/MutableDB with the given service injected on its
  // own MutableDB instance (the per-elaboration seam; no global state is touched)
  def genHostCached(service: SubDesignCache): dfhdl.core.Design =
    given options.ElaborationOptions.CacheEnable = true
    val dfc = DFC.empty(summon[options.ElaborationOptions])
    dfc.mutableDB.DesignLoadGate.subDesignCache = service
    genHost(using dfc)

  val expectedCodeString =
    """|def topCalc(arg: UInt[8] <> VAL): UInt[8] <> DFRET =
       |  arg + d"8'1"
       |end topCalc
       |
       |class Host extends DFDesign:
       |  val data = UInt(8) <> IN
       |  val w: UInt[8] <> CONST = d"8'7"
       |  val o = UInt(8) <> OUT
       |  val t = UInt(8) <> OUT
       |  def calc(arg: UInt[8] <> VAL): UInt[8] <> DFRET =
       |    arg + w
       |  end calc
       |  o := calc(data)
       |  t := topCalc(data)
       |end Host
       |""".stripMargin

  test("sub-design cache round trip: a fabricated hit elaborates like a live run") {
    val cache = new MapSubDesignCache
    // first elaboration runs live and stores the cached DBs at DB finalization
    assertCodeString(genHostCached(cache), expectedCodeString)
    assertEquals(cache.hits, 0)
    assertEquals(cache.entries.size, 2) // `calc` and `topCalc`
    // second elaboration hits the cache: the def body elaborations are skipped and
    // the cached DBs are attached, producing the exact same code
    assertCodeString(genHostCached(cache), expectedCodeString)
    assertEquals(cache.hits, 2)
    // the attached DB behaves through compiler stages like a live one
    assertCodeString(genHostCached(cache).dropDesignDefs, expectedDroppedCodeString)
    assertEquals(cache.hits, 4)
  }

  val expectedDroppedCodeString =
    """|class calc(val w: UInt[8] <> CONST) extends DFDesign:
       |  val arg = UInt(8) <> IN
       |  val o = UInt(8) <> OUT
       |  o <> (arg + w)
       |end calc
       |
       |class topCalc extends DFDesign:
       |  val arg = UInt(8) <> IN
       |  val o = UInt(8) <> OUT
       |  o <> (arg + d"8'1")
       |end topCalc
       |
       |class Host extends DFDesign:
       |  val data = UInt(8) <> IN
       |  val w: UInt[8] <> CONST = d"8'7"
       |  val o = UInt(8) <> OUT
       |  val t = UInt(8) <> OUT
       |  val o_part_calc_inst = calc(w = w)
       |  o_part_calc_inst.arg <> data
       |  o := o_part_calc_inst.o
       |  val o_part_topCalc_inst = topCalc()
       |  o_part_topCalc_inst.arg <> data
       |  t := o_part_topCalc_inst.o
       |end Host
       |""".stripMargin

  test("the disk service stores beside the owner class build output and hits") {
    // the spec's classes compile into a directory classpath entry (test-classes), so
    // the disk cache folder sits beside it; clean it for a deterministic first miss
    val classesDir = java.nio.file.Paths.get(
      classOf[SubDesignCacheSpec].getProtectionDomain.getCodeSource.getLocation.toURI
    )
    val cacheDir = classesDir.resolveSibling("dfhdl-cache")
    if (java.nio.file.Files.exists(cacheDir))
      java.nio.file.Files.list(cacheDir).forEach(java.nio.file.Files.delete(_))
    // the disk service keeps a process-wide in-memory store; drop it so the first
    // elaboration deterministically misses even in a long-lived sbt session
    SubDesignDiskCache.clearInMemoryStore()
    val cache = new CountingSubDesignCache(new SubDesignDiskCache)
    assertCodeString(genHostCached(cache), expectedCodeString)
    assertEquals(cache.hits, 0)
    assertEquals(cache.stores, 2)
    assert(java.nio.file.Files.list(cacheDir).count() == 2)
    assertCodeString(genHostCached(cache), expectedCodeString)
    assertEquals(cache.hits, 2)
  }

  test("without cacheEnable the elaboration is unaffected") {
    assertCodeString(genHost(using DFC.emptyNoEO), expectedCodeString)
    // the live dropped view matches the cached one asserted above
    assertCodeString(genHost(using DFC.emptyNoEO).dropDesignDefs, expectedDroppedCodeString)
  }
end SubDesignCacheSpec
