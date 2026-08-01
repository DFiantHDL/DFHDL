package StagesSpec

import dfhdl.*
import dfhdl.compiler.ir.*
import dfhdl.compiler.patching.*
import dfhdl.compiler.stages.sanityCheck
// scalafmt: { align.tokens = [{code = "<>"}, {code = "="}, {code = "=>"}, {code = ":="}]}

// `SanityCheck` guards what a *stage* may produce, so its inputs are DBs no design can express:
// each test elaborates a valid design and then breaks its sub-DB the way a faulty stage would.
class SanityCheckSpec extends StageSpec:
  class Top extends DFDesign:
    val x = SInt(16) <> IN
    val y = SInt(16) <> OUT
    y := x + 1
  end Top

  private def breakSubDB(db: DB)(f: DB => DB): DB =
    val (key, sub) = db.subDBs.head
    db.update(subDBs = db.subDBs.updated(key, f(sub)))

  test("a member referencing a later member fails the order check") {
    val broken = breakSubDB((new Top).getDB) { sub =>
      val members = sub.members
      val net     = members.collectFirst { case n: DFNet => n }.get
      // the assignment moves ahead of everything it reads, which is what a relocating stage does
      // when it leaves a dependency behind
      val (before, after) = members.filterNot(_ eq net).splitAt(members.indexOf(sub.top) + 1)
      sub.update(members = before ::: net :: after)
    }
    val err = intercept[IllegalArgumentException](broken.sanityCheck)
    assert(clue(err.getMessage).contains("Failed member order check!"))
  }

  test("a broken per-design elaboration check is caught between stages") {
    val broken = breakSubDB((new Top).getDB) { sub =>
      given MemberGetSet = sub.getSet
      val y = sub.members.collectFirst { case dcl: DFVal.Dcl if dcl.getName == "y" => dcl }.get
      sub.patch(List(y -> Patch.Replace(y.anonymize, Patch.Replace.Config.FullReplacement)))
    }
    val err = intercept[IllegalArgumentException](broken.sanityCheck)
    assert(clue(err.getMessage).contains("DFiant HDL name errors!"))
  }
end SanityCheckSpec
