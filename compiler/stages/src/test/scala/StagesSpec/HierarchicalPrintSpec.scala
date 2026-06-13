package StagesSpec

import dfhdl.*
import dfhdl.compiler.printing.DefaultPrinter
// scalafmt: { align.tokens = [{code = "<>"}, {code = "="}, {code = "=>"}, {code = ":="}] }

// Validates that the printer produces identical output whether given a flat DB
// or its hierarchical (root + sub-DBs) form via `oldToNew`. This exercises the
// root-aware printer path (post-order `designPrinters`, per-sub-DB getSet
// routing, cross-design globals/named-types). The compiler pipeline still feeds
// the printer a flat DB, so without this the root path would be unexercised.
class HierarchicalPrintSpec extends StageSpec:
  private def assertSamePrintFlatVsHier(dsn: core.Design): Unit =
    val db   = dsn.getDB
    val flat = DefaultPrinter(using db.getSet).csDB
    val hier = DefaultPrinter(using db.oldToNew.getSet).csDB
    assertNoDiff(hier, flat)

  test("nested hierarchy: flat and hierarchical printing match") {
    class SubDesign extends DFDesign:
      val x = SInt(16) <> IN
      val y = SInt(16) <> OUT
      y := x

    class TopDesign extends DFDesign:
      val x   = SInt(16) <> IN
      val y   = SInt(16) <> OUT
      val sub = new SubDesign()
      sub.x <> x
      y     <> sub.y

    assertSamePrintFlatVsHier(new TopDesign)
  }
end HierarchicalPrintSpec
