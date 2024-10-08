package StagesSpec

import dfhdl.*
import dfhdl.compiler.stages.dropBinds
// scalafmt: { align.tokens = [{code = "<>"}, {code = "="}, {code = "=>"}, {code = ":="}]}

class DropBindsSpec extends StageSpec(stageCreatesUnrefAnons = true):
  test("Drop binds") {
    class ID extends DFDesign:
      case class Packet(header: Bits[8] <> VAL, cnt: UInt[8] <> VAL) extends Struct
      val x = Bits(16) <> IN
      val y = Packet   <> IN
      val z = Bits(8)  <> OUT
      x match
        case b"1000${hi: B[4]}10001000" =>
          z := hi.resize(8)
        case h"FB${hi: B[4]}E" =>
      x match
        case b"1000${same: B[4]}10001000" =>
        case h"F${same: B[4]}BE"          =>
//      y match
//        case Packet(all(0), z) if z - z.prev < 20 =>
//        case Packet(all(1), z) if z - z.prev < 20 =>
//        case _                                    =>
//      y match
//        case Packet(all(0), z1) if z1 - z1.prev < 20 =>
//        case Packet(all(1), z2) if z2 - z2.prev < 20 =>
//        case _                                       =>
      (x, y) match
        case (h"FB${hi: B[4]}E", Packet(b"10${there: B[4]}01", 55)) if hi == there =>
        case _                                                                     =>
    end ID

    val id = (new ID).dropBinds
    assertCodeString(
      id,
      """|final case class Packet(
         |    header: Bits[8] <> VAL
         |    cnt: UInt[8] <> VAL
         |) extends Struct
         |
         |class ID extends DFDesign:
         |  val x = Bits(16) <> IN
         |  val y = Packet <> IN
         |  val z = Bits(8) <> OUT
         |  val hi = x(11, 8)
         |  val hi = x(7, 4)
         |  x match
         |    case h"8?88" => z := hi.resize(8)
         |    case h"fb?e" =>
         |  end match
         |  val same = x(11, 8)
         |  x match
         |    case h"8?88" =>
         |    case h"f?be" =>
         |  end match
         |  val hi = x(7, 4)
         |  val there = y.header(5, 2)
         |  (x, y) match
         |    case (h"fb?e", Packet(b"10????01", d"8'55")) if hi == there =>
         |    case _ =>
         |  end match
         |end ID
         |""".stripMargin
    )
  }
end DropBindsSpec
