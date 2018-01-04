package DFiant.core

import DFiant.internals._
//
//sealed trait DFUInt extends DFAny.Val[WUnsafe, DFUInt, DFUInt.Var] {
//  def extBy(numOfBits : Int)     : TAlias = ???
//  def +  (that : DFUInt)         : DFUInt = ???
//  def -  (that : DFUInt)         : DFUInt = ???
//  def *  (that : DFUInt)         : DFUInt = ???
//  def /  (that : DFUInt)         : DFUInt = ???
//  def == (that : BigInt)         : DFBool = ???
//  def == (that : Int)            : DFBool = ???
//  def == (that : Long)           : DFBool = ???
//  def != (that : BigInt)         : DFBool = ???
//  def != (that : Int)            : DFBool = ???
//  def != (that : Long)           : DFBool = ???
//  def isZero                     : DFBool = ???
//  def isNonZero                  : DFBool = ???
//  def <  (that : DFUInt)         : DFBool = ???
//  def <  (that : BigInt)         : DFBool = ???
//  def <  (that : Int)            : DFBool = ???
//  def <  (that : Long)           : DFBool = ???
//  def >= (that : DFUInt)         : DFBool = ???
//  def >= (that : BigInt)         : DFBool = ???
//  def >= (that : Int)            : DFBool = ???
//  def >= (that : Long)           : DFBool = ???
//  def >  (that : DFUInt)         : DFBool = ???
//  def >  (that : BigInt)         : DFBool = ???
//  def >  (that : Int)            : DFBool = ???
//  def >  (that : Long)           : DFBool = ???
//  def <= (that : DFUInt)         : DFBool = ???
//  def <= (that : BigInt)         : DFBool = ???
//  def <= (that : Int)            : DFBool = ???
//  def <= (that : Long)           : DFBool = ???
//  def dfTypeName : String = "DFUInt"
//}
//
//
//object DFUInt {
//  case class Var(width : Int) extends DFAny.Var[WUnsafe, DFUInt, DFUInt.Var]() with DFUInt {
//    final def := (that : BigInt) : this.type = ???
//    final def := (that : Int) : this.type = ???
//    final def := (that : Long) : this.type = ???
//    def newEmptyDFVar = copy()
//  }
//
//  def apply(width : Int)            : Var = Var(width)
//  def rangeUntil(supLimit : Int)    : Var = rangeUntil(intToBigIntBits(supLimit))
//  def rangeUntil(supLimit : Long)   : Var = rangeUntil(longToBigIntBits(supLimit))
//  def rangeUntil(supLimit : BigInt) : Var = apply(bigIntRepWidth(supLimit-1))
//  def rangeTo(maxLimit : Int)       : Var = rangeTo(intToBigIntBits(maxLimit))
//  def rangeTo(maxLimit : Long)      : Var = rangeTo(longToBigIntBits(maxLimit))
//  def rangeTo(maxLimit : BigInt)    : Var = apply(bigIntRepWidth(maxLimit))
//}