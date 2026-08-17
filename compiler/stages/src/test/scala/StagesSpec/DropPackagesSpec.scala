package StagesSpec

import dfhdl.*
import dfhdl.compiler.stages.{dropPackages, getCodeString}
// scalafmt: { align.tokens = [{code = "<>"}, {code = "="}, {code = "=>"}, {code = ":="}]}

class DropPackagesSpec extends StageSpec:
  // the stage only runs for a backend that has no packages
  given options.CompilerOptions.Backend = _.verilog.v95

  // `GlbNsStruct`/`GlbNsConst` share the top design's namespace, so they are already
  // general-placed and keep their names; everything declared under `typespkg1`/`typespkg2`
  // takes its package name.
  // NOTE: the declaration of the flattened static function is not in this printout — the DFHDL
  // printer renders global HDL methods from the flat DB only — but its call sites carry the
  // flattened name. The declaration is pinned end-to-end in `PrintVerilogCodeSpec`'s
  // "Namespace-derived declarations flattened under verilog.v95".
  val flattenedCS =
    """|val GlbNsConst: UInt[8] <> CONST = d"8'3"
       |val typespkg1_PkgConst: UInt[8] <> CONST = GlbNsConst + d"8'39"
       |val typespkg1_PkgDerived: UInt[8] <> CONST = typespkg1_pkgCalc(typespkg1_PkgConst)
       |val typespkg2_PkgWide: UInt[8] <> CONST = typespkg1_pkgCalc(typespkg1_PkgDerived)
       |
       |class PkgTop extends DFDesign:
       |  final case class GlbNsStruct(
       |      g: Bits[2] <> VAL
       |  ) extends Struct
       |  final case class typespkg1_PkgStruct(
       |      a: Bits[8] <> VAL
       |      b: Bit <> VAL
       |      g: GlbNsStruct <> VAL
       |  ) extends Struct
       |  enum typespkg1_PkgEnum(val value: UInt[2] <> CONST) extends Encoded.Manual(2):
       |    case P0 extends typespkg1_PkgEnum(d"2'0")
       |    case P1 extends typespkg1_PkgEnum(d"2'1")
       |    case P2 extends typespkg1_PkgEnum(d"2'2")
       |  case class typespkg1_PkgOpaque() extends Opaque(Bits(4))
       |  final case class typespkg2_PkgWrap(
       |      s: typespkg1_PkgStruct <> VAL
       |      n: UInt[8] <> VAL
       |  ) extends Struct
       |
       |  val s = typespkg1_PkgStruct <> VAR
       |  val e = typespkg1_PkgEnum <> VAR
       |  val o = typespkg1_PkgOpaque <> VAR
       |  val w = typespkg2_PkgWrap <> VAR
       |  val u = UInt(8) <> VAR init typespkg2_PkgWide
       |  e := typespkg1_PkgEnum.P0
       |end PkgTop
       |""".stripMargin

  class PkgTop extends DFDesign:
    val s = typespkg1.PkgStruct <> VAR
    val e = typespkg1.PkgEnum   <> VAR
    val o = typespkg1.PkgOpaque <> VAR
    val w = typespkg2.PkgWrap   <> VAR
    val u = UInt(8)             <> VAR init typespkg2.PkgWide
    e := typespkg1.PkgEnum.P0

  test("packaged types, constants and static functions take their package name") {
    assertCodeString((new PkgTop).dropPackages, flattenedCS)
  }

  test("re-running changes nothing (the namespace goes with the rename)") {
    assertCodeString((new PkgTop).dropPackages.dropPackages, flattenedCS)
  }

  test("nothing is flattened for a backend that has packages") {
    given options.CompilerOptions.Backend = _.verilog.sv2009
    val top                               = (new PkgTop).dropPackages
    assert(clue(top.getCodeString).contains("package StagesSpec.typespkg1:"))
  }

end DropPackagesSpec
