package StagesSpec

import dfhdl.*
import dfhdl.compiler.stages.namedAnonCondExpr
// scalafmt: { align.tokens = [{code = "<>"}, {code = "="}, {code = "=>"}, {code = ":="}]}

class NamedAnonCondExprSpec extends StageSpec:
  // a conditional expression that already drives a declaration, or that a surrounding construct
  // carries through its own lowering, needs no name of its own
  test("conditional expressions that need no name") {
    // the assignments live in a process because an ED domain body is concurrent, where `:=` is
    // not allowed at all
    class Top extends EDDesign:
      val a, b = UInt(8) <> IN
      val c    = Bit     <> IN
      val y    = UInt(8) <> OUT
      val z    = UInt(8) <> VAR
      // connected to an output port
      y <> (if (c) a else b)
      process(all):
        // assigned to a declaration
        z := (if (c) a else b)
        // the result of a branch of an enclosing conditional expression
        val nested: UInt[8] <> VAL =
          if (c) (if (a > b) a else b)
          else d"8'0"
        z := nested
    end Top
    val result = (new Top).namedAnonCondExpr
    assertCodeString(
      result,
      """|class Top extends EDDesign:
         |  val a = UInt(8) <> IN
         |  val b = UInt(8) <> IN
         |  val c = Bit <> IN
         |  val y = UInt(8) <> OUT
         |  val z = UInt(8) <> VAR
         |  y <> ((
         |    if (c) a
         |    else b
         |  ): UInt[8] <> VAL)
         |  process(all):
         |    z := ((
         |      if (c) a
         |      else b
         |    ): UInt[8] <> VAL)
         |    val nested: UInt[8] <> VAL =
         |      if (c)
         |        if (a > b) a
         |        else b
         |      else d"8'0"
         |      end if
         |    z := nested
         |end Top
         |""".stripMargin
    )
  }

  // an HDL method's return wiring is an ident, like a branch result, but nothing downstream
  // lowers a conditional under it, so it has to be named here
  test("an ED method's returned conditional expression is named") {
    class Top extends EDDesign:
      val a, b                                                         = UInt(8) <> IN
      val c                                                            = Bit     <> IN
      val y                                                            = UInt(8) <> OUT
      def pick(l: UInt[8] <> VAL, r: UInt[8] <> VAL): UInt[8] <> EDRET =
        if (c) l
        else r
      y <> pick(a, b)
    end Top
    val result = (new Top).namedAnonCondExpr
    assertCodeString(
      result,
      """|class Top extends EDDesign:
         |  val a = UInt(8) <> IN
         |  val b = UInt(8) <> IN
         |  val c = Bit <> IN
         |  val y = UInt(8) <> OUT
         |  def pick(l: UInt[8] <> VAL, r: UInt[8] <> VAL): UInt[8] <> EDRET =
         |    val anon: UInt[8] <> VAL =
         |      if (c) l
         |      else r
         |    anon
         |  end pick
         |
         |  y <> pick(a, b)
         |end Top
         |""".stripMargin
    )
  }
end NamedAnonCondExprSpec
