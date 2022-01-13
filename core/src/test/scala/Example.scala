import DFiant.*
class Example(using DFC) extends DFDesign:
  case class Packet(
      header: DFBits[8] <> VAL,
      cnt: DFUInt[8] <> VAL,
      data: DFBits[8] <> VAL,
      checksum: DFBits[8] <> VAL
  ) extends DFStruct
  val i = Packet <> IN

  val Packet(h"CC", ccCnt, h"F${fData: B[4]}", _) = i
//  sim.report(msg"Found 0xCC packet with cnt $ccCnt and fData $fData")
