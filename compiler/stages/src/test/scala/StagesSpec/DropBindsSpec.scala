package StagesSpec

import DFiant.*
import DFiant.compiler.stages.dropBinds
// scalafmt: { align.tokens = [{code = "<>"}, {code = "="}, {code = "=>"}, {code = ":="}]}

class DropBindsSpec extends StageSpec:
  test("Drop binds") {
    class ID(using DFC) extends DFDesign:
      case class Packet(header: DFBits[8] <> VAL, cnt: DFUInt[8] <> VAL) extends DFStruct
      val x = DFBits(16) <> IN
      val y = Packet     <> IN
      val z = DFBits(8)  <> OUT
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
         |    header: DFBits[8] <> VAL
         |    cnt: DFUInt[8] <> VAL
         |) extends DFStruct
         |
         |class ID(using DFC) extends DFDesign:
         |  val x = DFBits(16) <> IN
         |  val y = Packet <> IN
         |  val z = DFBits(8) <> OUT
         |  val hi = x(12, 9)
         |  val hi = x(8, 5)
         |  x match
         |    case h"16'8?88" => z := hi.resize(8)
         |    case h"16'fb?e" =>
         |  val same = x(12, 9)
         |  x match
         |    case h"16'8?88" =>
         |    case h"16'f?be" =>
         |  val hi = x(8, 5)
         |  val there = y.header(6, 3)
         |  (x, y) match
         |    case (h"16'fb?e", Packet(b"10????01", d"8'55")) if hi == there =>
         |    case _ =>
         |end ID
         |""".stripMargin
    )
  }
end DropBindsSpec
