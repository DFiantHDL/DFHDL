package StagesSpec

import dfhdl.*
import dfhdl.compiler.stages.noLocalDcls
// scalafmt: { align.tokens = [{code = "<>"}, {code = "="}, {code = "=>"}, {code = ":="}]}

class NoLocalDclsSpec extends StageSpec:
  test("Nested local dcl move") {
    class ID extends DFDesign:
      val x = SInt(16) <> IN
      val y = SInt(16) <> OUT
      val z = SInt(16) <> VAR
      z := x
      if (x > 5)
        val zz = SInt(16) <> VAR
        x match
          case 2 =>
            val zzz = SInt(16) <> VAR init 0
            val c   = SInt(16) const 1
            zzz := zzz.prev(1) + c
          case _ =>
        zz := x
        z  := zz
      y := z
    end ID
    val id = (new ID).noLocalDcls
    assertCodeString(
      id,
      """|class ID extends DFDesign:
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT
         |  val z = SInt(16) <> VAR
         |  z := x
         |  val zz = SInt(16) <> VAR
         |  val zzz = SInt(16) <> VAR init sd"16'0"
         |  val c = SInt(16) const sd"16'1"
         |  if (x > sd"16'5")
         |    x match
         |      case sd"16'2" => zzz := zzz.prev + c
         |      case _ =>
         |    zz := x
         |    z := zz
         |  y := z
         |end ID
         |""".stripMargin
    )
  }
end NoLocalDclsSpec
