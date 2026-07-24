package StagesSpec

import dfhdl.*
import dfhdl.compiler.ir
import dfhdl.core.{DFC, SubDesignCache}
// scalafmt: { align.tokens = [{code = "<>"}, {code = "="}, {code = "=>"}, {code = ":="}]}

// Counts design-class body elaborations. A top-level (static) object is not a class-template
// capture, so it stays out of the design load key and only observes what actually ran. The count
// lives in a Java atomic: the purity analysis reads Scala code, and a `var` write it CAN see
// (even a few calls deep) makes the design impure, hence unkeyable and never loadable, which is
// exactly what these tests must not do to their subjects.
object ClassBodyElaborations:
  private val n     = java.util.concurrent.atomic.AtomicInteger(0)
  def tick(): Unit  = n.incrementAndGet()
  def count: Int    = n.get()
  def reset(): Unit = n.set(0)

/** Tests for the class-design body-skip rigging of the elaboration design load gate: the compiler
  * plugin guards a design class's body statements with the gate's decision, leaving the class's
  * public interface (its ports, constants and interfaces) unguarded, so a design that the gate can
  * load (this run's canonical of the same key, or an adopted sub-design cache entry) never
  * elaborates its body at all. The design instance binds the same ports and applied parameters
  * either way, so a loaded design must produce the exact same code as a live one.
  */
class ClassDesignCacheSpec extends StageSpec:
  // a map-backed cache service that round-trips entries through their JSON serialization
  class MapSubDesignCache extends SubDesignCache:
    val entries = collection.mutable.Map.empty[(Class[?], String), String]
    var hits    = 0
    def lookup(ownerClass: Class[?], localKey: String): Option[ir.SubDesignEntry] =
      val res = entries.get((ownerClass, localKey)).map(ir.SubDesignEntry.fromJsonString(_))
      if (res.nonEmpty) hits += 1
      res
    def store(ownerClass: Class[?], localKey: String, entry: ir.SubDesignEntry): Unit =
      entries((ownerClass, localKey)) = entry.toJsonString
  end MapSubDesignCache

  // the cache tier is explicitly configured on both sides, so this spec asserts the gate's
  // behavior itself and does not ride on whatever `CacheEnable` defaults to
  def cachedDFC(cache: SubDesignCache): DFC =
    given options.ElaborationOptions.CacheEnable = true
    val dfc                                      = DFC.empty(summon[options.ElaborationOptions])
    dfc.mutableDB.DesignLoadGate.subDesignCache = cache
    dfc
  def liveDFC: DFC =
    given options.ElaborationOptions.CacheEnable = false
    DFC.empty(summon[options.ElaborationOptions])

  // ~~~ a design-class hierarchy: the whole forest must load from one adopted top entry ~~~
  def genChainHost(using DFC): dfhdl.core.Design =
    class Leaf extends DFDesign:
      val i = UInt(8) <> IN
      val o = UInt(8) <> OUT
      ClassBodyElaborations.tick()
      o := i + 1
    class Mid extends DFDesign:
      val i    = UInt(8) <> IN
      val o    = UInt(8) <> OUT
      val leaf = Leaf()
      ClassBodyElaborations.tick()
      leaf.i <> i
      o      <> leaf.o
    class ChainHost extends DFDesign:
      val i   = UInt(8) <> IN
      val o   = UInt(8) <> OUT
      val mid = Mid()
      mid.i <> i
      o     <> mid.o
    new ChainHost
  end genChainHost

  val expectedChainCodeString =
    """|class Leaf extends DFDesign:
       |  val i = UInt(8) <> IN
       |  val o = UInt(8) <> OUT
       |  o := i + d"8'1"
       |end Leaf
       |
       |class Mid extends DFDesign:
       |  val i = UInt(8) <> IN
       |  val o = UInt(8) <> OUT
       |  val leaf = Leaf()
       |  leaf.i <> i
       |  o <> leaf.o
       |end Mid
       |
       |class ChainHost extends DFDesign:
       |  val i = UInt(8) <> IN
       |  val o = UInt(8) <> OUT
       |  val mid = Mid()
       |  mid.i <> i
       |  o <> mid.o
       |end ChainHost
       |""".stripMargin

  test("a cached class-design hierarchy adopts its whole forest") {
    val cache = new MapSubDesignCache
    ClassBodyElaborations.reset()
    assertCodeString(genChainHost(using liveDFC), expectedChainCodeString)
    assertEquals(ClassBodyElaborations.count, 2) // `Mid` and `Leaf` bodies ran
    // the first cached elaboration runs live and stores `Mid` and `Leaf`
    ClassBodyElaborations.reset()
    assertCodeString(genChainHost(using cachedDFC(cache)), expectedChainCodeString)
    assertEquals(ClassBodyElaborations.count, 2)
    assertEquals(cache.entries.size, 2)
    // the second one adopts `Mid` (and, through its entry's child key, `Leaf`): no body
    // elaborates, and the design prints exactly as the live one
    ClassBodyElaborations.reset()
    assertCodeString(genChainHost(using cachedDFC(cache)), expectedChainCodeString)
    assertEquals(ClassBodyElaborations.count, 0)
  }

  // ~~~ intra-run: the same class instantiated twice elaborates ONE body ~~~
  def genTwiceHost(using DFC): dfhdl.core.Design =
    class Sub extends DFDesign:
      val i = UInt(8) <> IN
      val o = UInt(8) <> OUT
      ClassBodyElaborations.tick()
      o := i + 1
    class TwiceHost extends DFDesign:
      val i = UInt(8) <> IN
      val o = UInt(8) <> OUT
      val a = Sub()
      val b = Sub()
      a.i <> i
      b.i <> a.o
      o   <> b.o
    new TwiceHost
  end genTwiceHost

  val expectedTwiceCodeString =
    """|class Sub extends DFDesign:
       |  val i = UInt(8) <> IN
       |  val o = UInt(8) <> OUT
       |  o := i + d"8'1"
       |end Sub
       |
       |class TwiceHost extends DFDesign:
       |  val i = UInt(8) <> IN
       |  val o = UInt(8) <> OUT
       |  val a = Sub()
       |  val b = Sub()
       |  a.i <> i
       |  b.i <> a.o
       |  o <> b.o
       |end TwiceHost
       |""".stripMargin

  test("a repeated class instantiation elaborates one body, cache or no cache") {
    // the intra-run tier of the gate needs no cache service: the second instantiation
    // reuses this run's canonical design and skips its body
    ClassBodyElaborations.reset()
    assertCodeString(genTwiceHost(using liveDFC), expectedTwiceCodeString)
    assertEquals(ClassBodyElaborations.count, 1)
  }

  // ~~~ a parametrized design class: one body, per-instance applied values ~~~
  def genParamHost(using DFC): dfhdl.core.Design =
    class Add(val amount: UInt[8] <> CONST) extends DFDesign:
      val i = UInt(8) <> IN
      val o = UInt(8) <> OUT
      ClassBodyElaborations.tick()
      o := i + amount
    class ParamHost extends DFDesign:
      val i  = UInt(8) <> IN
      val o  = UInt(8) <> OUT
      val a1 = Add(1)
      val a2 = Add(2)
      a1.i <> i
      a2.i <> a1.o
      o    <> a2.o
    new ParamHost
  end genParamHost

  val expectedParamCodeString =
    """|class Add(val amount: UInt[8] <> CONST) extends DFDesign:
       |  val i = UInt(8) <> IN
       |  val o = UInt(8) <> OUT
       |  o := i + amount
       |end Add
       |
       |class ParamHost extends DFDesign:
       |  val i = UInt(8) <> IN
       |  val o = UInt(8) <> OUT
       |  val a1 = Add(amount = d"8'1")
       |  val a2 = Add(amount = d"8'2")
       |  a1.i <> i
       |  a2.i <> a1.o
       |  o <> a2.o
       |end ParamHost
       |""".stripMargin

  test("a cached parametrized class design binds per-instance applied values") {
    val cache = new MapSubDesignCache
    // the applied parameter values are NOT part of the key (a pure body cannot depend on
    // them), so both instances share one loaded body and only their bindings differ
    ClassBodyElaborations.reset()
    assertCodeString(genParamHost(using cachedDFC(cache)), expectedParamCodeString)
    assertEquals(ClassBodyElaborations.count, 1)
    ClassBodyElaborations.reset()
    assertCodeString(genParamHost(using cachedDFC(cache)), expectedParamCodeString)
    assertEquals(ClassBodyElaborations.count, 0)
  }
end ClassDesignCacheSpec
