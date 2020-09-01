package experiment

import DFiant._

import internals.IntExtras
import internals.BigIntExtras
import internals.BitVectorExtras
trait PrioEnc extends DFDesign {
  lazy val lines = 128
  final lazy val width = lines.bitsWidth
  val wire = DFBits(lines) <> IN
  val wor = DFBits(width) <> OUT

//  wor := b0s
//  var bv = BigInt(1)
//  val myIf = ifdf(wire == 1.toBitVector(128)) {
//    wor := b0s
//  }
//  (0 until 128).foldLeft(myIf)((b, i) => b.elseifdf(wire(i) {wor := i.toBitVector(width)}))
}

//object PrioEncApp extends DFApp.VHDLCompiler[PrioEnc]
