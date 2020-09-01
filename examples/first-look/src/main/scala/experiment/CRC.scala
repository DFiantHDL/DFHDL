//package fpga2020
//import DFiant._
//
//import internals.BitVectorExtras
//
//abstract class CRC(polynomial : BitVector, initValue : BitVector, dw : Int)(implicit ctx : DFDesign.ContextOf[CRC]) extends DFDesign {
//  val pw = polynomial.length.toInt
//  val dataI   = DFBits(dw) <> IN
//  val matchO  = DFBool()          <> OUT
//  val crcO    = DFBits(pw) <> OUT
//
//  val msb = pw-1
//  val init_msb = initValue.length.toInt-1
//
//
//  assert(msb == init_msb, "polynomial and initValue vectors must be equal length!")
//  assert(msb>=3 && msb <=31, "polynomial must be of order 4 to 32!")
//  assert(polynomial.bit(0), "polynomial must have lsb set to 1!")
//
//  val crca = List.fill(dw)(DFBits(pw))
//
//}
