package StagesSpec

import dfhdl.*
import dfhdl.compiler.ir
import dfhdl.compiler.stages.dropDFMethods
import dfhdl.compiler.printing.DefaultPrinter
import dfhdl.core.{DFC, SubDesignCache, SubDesignDiskCache}
// scalfmt: { align.tokens = [{code = "<>"}, {code = "="}, {code = "=>"}, {code = ":="}]}

// a TOP-LEVEL method: its owner class is the synthetic `<file>$package` class, so
// the disk cache entry lives beside this file's build output like any other class
def topCalc(arg: UInt[8] <> VAL): UInt[8] <> DFRET =
  arg + 1

// two top-level defs, each cached from its OWN elaboration (see the colliding-tokens test)
def topCalcA(arg: UInt[8] <> VAL): UInt[8] <> DFRET = (arg + 2) * 3
def topCalcB(arg: UInt[8] <> VAL): UInt[8] <> DFRET = (arg - 4) * 5

/** Tests for the sub-design cache tier of the elaboration design load gate
  * (`ElaborationOptions.CacheEnable`): a pure method whose cached DB is found by the
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
    def lookup(ownerClass: Class[?], localKey: String): Option[ir.SubDesignEntry] =
      val res = entries.get((ownerClass, localKey)).map(ir.SubDesignEntry.fromJsonString(_))
      if (res.nonEmpty) hits += 1
      res
    def store(ownerClass: Class[?], localKey: String, entry: ir.SubDesignEntry): Unit =
      entries((ownerClass, localKey)) = entry.toJsonString
  end MapSubDesignCache

  // wraps the real disk service with hit/store counters
  class CountingSubDesignCache(underlying: SubDesignCache) extends SubDesignCache:
    var hits = 0
    var stores = 0
    def lookup(ownerClass: Class[?], localKey: String): Option[ir.SubDesignEntry] =
      val res = underlying.lookup(ownerClass, localKey)
      if (res.nonEmpty) hits += 1
      res
    def store(ownerClass: Class[?], localKey: String, entry: ir.SubDesignEntry): Unit =
      stores += 1
      underlying.store(ownerClass, localKey, entry)
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

  // the cache tier is explicitly configured on both sides here, so this spec asserts the
  // gate's behavior itself and does not ride on whatever `CacheEnable` defaults to
  def cachedDFC: DFC =
    given options.ElaborationOptions.CacheEnable = true
    DFC.empty(summon[options.ElaborationOptions])
  def liveDFC: DFC =
    given options.ElaborationOptions.CacheEnable = false
    DFC.empty(summon[options.ElaborationOptions])

  // each elaboration gets a fresh DFC/MutableDB with the given service injected on its
  // own MutableDB instance (the per-elaboration seam; no global state is touched)
  def genHostCached(service: SubDesignCache): dfhdl.core.Design =
    genHostOf(genHost, service)
  // elaborates a design under a fresh cache-enabled DFC with the given service injected
  def genHostOf(gen: DFC ?=> dfhdl.core.Design, service: SubDesignCache): dfhdl.core.Design =
    val dfc = cachedDFC
    dfc.mutableDB.DesignLoadGate.subDesignCache = service
    gen(using dfc)

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
    assertCodeString(genHostCached(cache).dropDFMethods, expectedDroppedCodeString)
    assertEquals(cache.hits, 4)
  }

  // Method (ED method / static function) calls are `Func`/`Op.Def` expressions rather
  // than design instances, and a static function calling another (quad -> twice) makes the
  // cached `quad` entry carry a call whose design key must be re-anchored at adoption
  // (`SubDesignEntry.cloneForAdoption`).
  test("sub-design cache round trip: method calls (ED method + static functions)") {
    def genEDHost(using DFC): dfhdl.core.Design =
      class EDHost extends EDDesign:
        val a = UInt(8) <> IN
        val o = UInt(8) <> OUT
        val s = UInt(8) <> OUT
        def add1(l: UInt[8] <> VAL): UInt[8] <> EDRET = l + 1
        def twice(k: UInt[8] <> CONST): UInt[8] <> CONSTRET = k + k
        def quad(n: UInt[8] <> CONST): UInt[8] <> CONSTRET = twice(twice(n))
        o <> add1(a)
        s <> quad(d"8'3")
      end EDHost
      new EDHost
    val expected =
      """|class EDHost extends EDDesign:
         |  val a = UInt(8) <> IN
         |  val o = UInt(8) <> OUT
         |  val s = UInt(8) <> OUT
         |  def add1(l: UInt[8] <> VAL): UInt[8] <> EDRET =
         |    l + d"8'1"
         |  end add1
         |
         |  def twice(k: UInt[8] <> CONST): UInt[8] <> CONSTRET =
         |    k + k
         |  end twice
         |
         |  def quad(n: UInt[8] <> CONST): UInt[8] <> CONSTRET =
         |    twice(twice(n))
         |  end quad
         |
         |  o <> add1(a)
         |  s <> quad(d"8'3")
         |end EDHost
         |""".stripMargin
    val cache = new MapSubDesignCache
    // first elaboration runs live and stores `add1`, `twice`, and `quad`
    assertCodeString(genHostOf(genEDHost, cache), expected)
    assertEquals(cache.hits, 0)
    assertEquals(cache.entries.size, 3)
    // second elaboration: `add1` and `quad` hit at their call sites; `twice` (called only
    // inside `quad`'s cached body) hits through `quad`'s child adoption
    assertCodeString(genHostOf(genEDHost, cache), expected)
    assertEquals(cache.hits, 3)
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
    // every other spec in this JVM elaborates with the cache tier on and stores into this same
    // folder, concurrently: an entry (or a store's temp file) that vanishes mid-wipe is expected
    if (java.nio.file.Files.exists(cacheDir))
      java.nio.file.Files.list(cacheDir).forEach { entry =>
        try java.nio.file.Files.deleteIfExists(entry)
        catch case scala.util.control.NonFatal(_) => ()
      }
    // the disk service keeps a process-wide in-memory store; drop it so the first
    // elaboration deterministically misses even in a long-lived sbt session
    SubDesignDiskCache.clearInMemoryStore()
    val cache = new CountingSubDesignCache(new SubDesignDiskCache)
    assertCodeString(genHostCached(cache), expectedCodeString)
    assertEquals(cache.hits, 0)
    assertEquals(cache.stores, 2)
    // at least the two defs of this elaboration; other tests elaborating in the same
    // JVM share this build output's cache folder and may add their own entries
    assert(java.nio.file.Files.list(cacheDir).count() >= 2)
    assertCodeString(genHostCached(cache), expectedCodeString)
    assertEquals(cache.hits, 2)
  }

  // a method whose own body instantiates methods: a cached hit on `outer` must
  // adopt the whole forest (`outer` and its `inner` children) with all connections intact
  def genNestHost(using DFC): dfhdl.core.Design =
    class NestHost extends DFDesign:
      val data = UInt(8) <> IN
      val o = UInt(8) <> OUT
      def inner(arg: UInt[8] <> VAL): UInt[8] <> DFRET =
        arg + 1
      def outer(arg: UInt[8] <> VAL): UInt[8] <> DFRET =
        val i = inner(arg)
        inner(i)
      o := outer(data)
    end NestHost
    new NestHost
  end genNestHost

  val expectedNestCodeString =
    """|def inner(arg: UInt[8] <> VAL): UInt[8] <> DFRET =
       |  arg + d"8'1"
       |end inner
       |
       |def outer(arg: UInt[8] <> VAL): UInt[8] <> DFRET =
       |  val i = inner(arg)
       |  inner(i)
       |end outer
       |
       |class NestHost extends DFDesign:
       |  val data = UInt(8) <> IN
       |  val o = UInt(8) <> OUT
       |  o := outer(data)
       |end NestHost
       |""".stripMargin

  test("a cached def that instantiates methods adopts its whole forest") {
    val cache = new MapSubDesignCache
    assertCodeString(genNestHost(using liveDFC), expectedNestCodeString)
    val dfc1 = cachedDFC
    dfc1.mutableDB.DesignLoadGate.subDesignCache = cache
    assertCodeString(genNestHost(using dfc1), expectedNestCodeString)
    val dfc2 = cachedDFC
    dfc2.mutableDB.DesignLoadGate.subDesignCache = cache
    assertCodeString(genNestHost(using dfc2), expectedNestCodeString)
  }

  // two cached defs that each instantiate the SAME leaf def: adopting both artifacts
  // brings in two copies of the leaf's cached sub-DB
  def genSharedLeafHost(using DFC): dfhdl.core.Design =
    class SharedLeafHost extends DFDesign:
      val data = UInt(8) <> IN
      val o = UInt(8) <> OUT
      def leaf(arg: UInt[8] <> VAL): UInt[8] <> DFRET =
        arg + 1
      def a(arg: UInt[8] <> VAL): UInt[8] <> DFRET =
        leaf(arg) + 2
      def b(arg: UInt[8] <> VAL): UInt[8] <> DFRET =
        leaf(arg) + 3
      o := a(data) + b(data)
    end SharedLeafHost
    new SharedLeafHost
  end genSharedLeafHost

  test("two cached defs sharing a leaf def adopt one leaf, not two") {
    val cache = new MapSubDesignCache
    val liveDB = genSharedLeafHost(using liveDFC).getDB
    val liveCS =
      import liveDB.getSet
      DefaultPrinter.csDB
    val dfc1 = cachedDFC
    dfc1.mutableDB.DesignLoadGate.subDesignCache = cache
    genSharedLeafHost(using dfc1)
    val dfc2 = cachedDFC
    dfc2.mutableDB.DesignLoadGate.subDesignCache = cache
    assertCodeString(genSharedLeafHost(using dfc2), liveCS)
  }

  // ~~~ tokens of an adopted design vs. the loading run's ~~~
  // A design adopted from the cache keeps the ref tokens the STORING run minted. Within one JVM
  // those can never collide with the loading run's: a token's `grpId` carries a process-wide
  // per-elaboration counter (`DFC.getGrpId`), so every elaboration mints in a namespace of its own.
  // ACROSS runs it is the opposite: a fresh JVM restarts that counter, so a run adopting an entry
  // stored by an earlier run mints in the SAME namespace, from id 1 — colliding by construction.
  //
  // So an entry is re-minted into the loading run's namespace as it is adopted
  // (`SubDesignEntry.cloneForAdoption`), and no stored token outlives the load. The collision is
  // not reproducible in-process, so it is handed to the adoption deliberately below, and the flat
  // view of a design adopting several entries is tested for soundness.
  def genOnlyA(using DFC): dfhdl.core.Design =
    class OnlyA extends DFDesign:
      val data = UInt(8) <> IN
      val o = UInt(8) <> OUT
      o := topCalcA(data)
    new OnlyA
  def genOnlyB(using DFC): dfhdl.core.Design =
    class OnlyB extends DFDesign:
      val data = UInt(8) <> IN
      val o = UInt(8) <> OUT
      o := topCalcB(data)
    new OnlyB
  def genBothHost(using DFC): dfhdl.core.Design =
    class BothHost extends DFDesign:
      val data = UInt(8) <> IN
      val o = UInt(8) <> OUT
      val t = UInt(8) <> OUT
      o := topCalcA(data)
      t := topCalcB(data)
    new BothHost

  test("the flat view of a design adopting several cached entries is sound") {
    val cache = new MapSubDesignCache
    // two SEPARATE elaborations store the two defs
    genHostOf(genOnlyA, cache)
    genHostOf(genOnlyB, cache)
    // one elaboration adopts both, and its flat view merges every sub-DB refTable into one
    val cachedFlat = genHostOf(genBothHost, cache).getDB.newToOld
    cachedFlat.check // every ref in the merged refTable resolves
    val cachedCS =
      import cachedFlat.getSet
      DefaultPrinter.csDB
    val liveFlat = genBothHost(using liveDFC).getDB.newToOld
    val liveCS =
      import liveFlat.getSet
      DefaultPrinter.csDB
    assertNoDiff(cachedCS, liveCS)
  }

  test("an adopted entry keeps no stored token, even when the run mints one of them") {
    val root = genBothHost(using liveDFC).getDB
    // a def's sub-DB: self-contained, with globals and structural keys reaching out of it
    val (_, sub) = root.subDBs.toList.last
    val entry = ir.SubDesignEntry(sub, Nil)
    given ir.RefGen = ir.RefGen.fromGetSet(using root.getSet)
    // THE CROSS-RUN CASE, reproduced in-process. A loading run mints its tokens from a generator
    // that restarts per run, so the token it mints for the adopted design's structural key can be
    // one the entry already uses internally -- and since both runs elaborate the same code, it
    // routinely IS. Here that worst case is handed over deliberately: the design block is anchored
    // on the very token a stored port uses for its owner.
    val storedPortOwnerRef = sub.members.collectFirst { case dcl: ir.DFVal.Dcl => dcl.ownerRef }.get
    val adopted = entry.cloneForAdoption(storedPortOwnerRef, Map.empty)
    // the design is anchored where the run asked, and every ref in it still resolves: nothing
    // collapsed onto the shared key (a port's owner, in particular, is still the design)
    assertEquals(ir.StaticRef(adopted.top.ownerRef), ir.StaticRef(storedPortOwnerRef))
    adopted.check
    val adoptedDcl = adopted.members.collectFirst { case dcl: ir.DFVal.Dcl => dcl }.get
    assertEquals(adoptedDcl.getOwner(using adopted.getSet), adopted.top: ir.DFMember)
    // ...and it elaborates to the same design
    val subCS =
      import sub.getSet
      DefaultPrinter.csDB
    val adoptedCS =
      import adopted.getSet
      DefaultPrinter.csDB
    assertNoDiff(adoptedCS, subCS)
    // no token the entry emitted survives into the loading run's namespace
    def localRefs(db: ir.DB): Set[ir.DFRefAny] =
      given ir.MemberGetSet = db.getSet
      db.members.view.collect {
        case g: ir.DFVal.CanBeGlobal if g.isGlobal => Set.empty[ir.DFRefAny]
        case d: ir.DFDesignBlock                   => d.getRefs.toSet
        case m                                     => m.getAllRefs.toSet
      }.flatten.toSet
    assertEquals(localRefs(sub).intersect(localRefs(adopted)), Set.empty[ir.DFRefAny])
  }

  test("without cacheEnable the elaboration is unaffected") {
    assertCodeString(genHost(using liveDFC), expectedCodeString)
    // the live dropped view matches the cached one asserted above
    assertCodeString(genHost(using liveDFC).dropDFMethods, expectedDroppedCodeString)
  }
end SubDesignCacheSpec
