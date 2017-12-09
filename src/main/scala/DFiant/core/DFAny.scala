package DFiant.core

//import DFiant.basiclib.TokensCounter
import DFiant.internals._
import singleton.twoface._

trait DFAny {
  type IN = TVal
  type OUT = TVar
  type TVal <: DFAny
  type TVar <: TVal with DFAny.Var[Width, TVal, TVar]
  type TAlias <: TVal
  type TBool <: DFBool
  type TBits[W2] <: DFBits[W2]
//  type TUInt <: DFUInt
  type Width
  val width : TwoFace.Int[Width]

  //////////////////////////////////////////////////////////////////////////
  // Single bit (Bool) selection
  //////////////////////////////////////////////////////////////////////////
  protected final def protBit[I](relBit : TwoFace.Int[I]) : TBool =
    DFBool.alias(this, relBit).asInstanceOf[TBool]

  final def bit[I](relBit : BitIndex.Checked[I, Width]) : TBool = protBit(relBit.unsafeCheck(width))
  final def bit[I](implicit relBit : BitIndex.Checked[I, Width], di : DummyImplicit) : TBool = protBit(relBit.unsafeCheck(width))
  //////////////////////////////////////////////////////////////////////////

  //////////////////////////////////////////////////////////////////////////
  // Bit range selection
  //////////////////////////////////////////////////////////////////////////
  final def bits() : TBits[Width] = DFBits.alias(this, width, 0).asInstanceOf[TBits[Width]]

  protected final def protBits[H, L](relBitHigh : TwoFace.Int[H], relBitLow : TwoFace.Int[L])(
    implicit relWidth : RelWidth.TF[H, L]
  ) : TBits[relWidth.Out] = DFBits.alias(this, relWidth(relBitHigh, relBitLow), relBitLow).asInstanceOf[TBits[relWidth.Out]]

  final def bits[H, L](relBitHigh : BitIndex.Checked[H, Width], relBitLow : BitIndex.Checked[L, Width])(
    implicit checkHiLow : BitsHiLo.Checked.Shell[H, L], relWidth : RelWidth.TF[H, L]
  ) = {
    checkHiLow.unsafeCheck(relBitHigh, relBitLow)
    protBits(relBitHigh.unsafeCheck(width), relBitLow.unsafeCheck(width))
  }

  final def bits[H, L](implicit relBitHigh : BitIndex.Checked[H, Width], relBitLow : BitIndex.Checked[L, Width],
    checkHiLow : BitsHiLo.Checked[H, L], relWidth : RelWidth.TF[H, L], di : DummyImplicit
  ) = protBits(relBitHigh.unsafeCheck(width), relBitLow.unsafeCheck(width))
  //////////////////////////////////////////////////////////////////////////

  //////////////////////////////////////////////////////////////////////////
  // Partial Bits at Position selection
  //////////////////////////////////////////////////////////////////////////
  protected final def protBitsWL[W, L](relWidth : TwoFace.Int[W], relBitLow : TwoFace.Int[L])
  : TBits[W] = DFBits.alias(this, relWidth, relBitLow).asInstanceOf[TBits[W]]

  import singleton.ops.-
  final def bitsWL[W, L](relWidth : TwoFace.Int[W], relBitLow : BitIndex.Checked[L, Width])(
    implicit checkRelWidth : PartWidth.Checked.Shell[W, Width - L]
  ) = {
    checkRelWidth.unsafeCheck(relWidth, width-relBitLow)
    protBitsWL(relWidth, relBitLow.unsafeCheck(width))
  }

  final def bitsWL[W, L](implicit relWidth : TwoFace.Int[W], relBitLow : BitIndex.Checked[L, Width],
    checkRelWidth : PartWidth.Checked.Shell[W, Width - L], di : DummyImplicit
  ) = {
    checkRelWidth.unsafeCheck(relWidth, width-relBitLow)
    protBitsWL(relWidth, relBitLow.unsafeCheck(width))
  }
  //////////////////////////////////////////////////////////////////////////

  //////////////////////////////////////////////////////////////////////////
  // Init (for use with Prev)
  //////////////////////////////////////////////////////////////////////////
  protected val protInit : DFInit[TVal] = DFInit.Bubble
  final def init : DFInit[TVal] = protInit
  final def init[R](value : DFInit.Able[TVal, R])(implicit bld : DFInit.Builder[TVal, R]) : TAlias =
    init(bld(this.asInstanceOf[TVal], value))
  def init(newInit : DFInit[TVal]) : TAlias = ???
  //////////////////////////////////////////////////////////////////////////

  //////////////////////////////////////////////////////////////////////////
  // Prev
  //////////////////////////////////////////////////////////////////////////
  final def prev(step : Int = 1) : TVal = ???
  //////////////////////////////////////////////////////////////////////////

  final def next(step : Int = 1) : TVal = ???
//  final def getNextSeq(seqNum : Int, slidingWindow : Boolean = false) : Seq[TVal] = {
//    val seq = Seq.tabulate(seqNum)(_ => this.newEmptyDFVar.dontProduce())
//    if (slidingWindow)
//      seq.zipWithIndex.foreach{case (e, i) => e := this.next(i)}
//    else {
//      ifdf (this.tokensCounter(seqNum) == 0) { //TODO: think about tokenCnt limit here (maybe seqNum-1 ??)
//        seq.zipWithIndex.foreach{case (e, i) => e := this.next(i)}
//      }
//    }
//    seq
//  }
  final def consume() : TAlias = {
    ???
    this.asInstanceOf[TAlias]
  }
  final def dontConsume() : TAlias = {
    ???
    this.asInstanceOf[TAlias]
  }
  final def isNotEmpty : DFBool = ???
//  final def tokensCounter(supremLimit : Int) : DFUInt = TokensCounter(this, supremLimit)
  def newEmptyDFVar : TVar
  def newCopyDFVar : TVar = newEmptyDFVar := this.asInstanceOf[TVal]

  protected[DFiant] lazy val almanacEntry : AlmanacEntry = AlmanacEntryCreateDFVar(width)

  protected[DFiant] final def getCurrentEntry : AlmanacEntry = AlmanacEntryGetDFVar(almanacEntry)

  protected[DFiant] final def assign(that : AlmanacEntry) : TVar = {
    AlmanacEntryAssign(this.almanacEntry, that)
    this.asInstanceOf[TVar]
  }
  protected[DFiant] final def assign(that : DFAny) : TVar = {
    AlmanacEntryAssign(this.almanacEntry, that.getCurrentEntry)
    this.asInstanceOf[TVar]
  }
  protected[DFiant] final def assign(that : BigInt) : TVar = {
    AlmanacEntryAssign(this.almanacEntry, AlmanacEntryConst(that))
    this.asInstanceOf[TVar]
  }
  final def == (that : TVal) : DFBool = ??? //DFBool.op(AlmanacEntryOpEq(this.getCurrentEntry, that.getCurrentEntry))
  final def != (that : TVal) : DFBool = ??? //!(this == that)
  def simInject(that : BigInt) : Boolean = almanacEntry.simInject(that)
  def simWatch : BigInt = ???
  def dfTypeName : String
  override def toString: String = s"$dfTypeName@$almanacEntry"
}

trait DFAnyW[W] extends DFAny {
  type Width = W
}


object DFAny {
  trait Val[W, Val0 <: DFAny, Var0 <: Val0 with DFAny.Var[W, Val0, Var0]] extends DFAnyW[W] {
    this : Val0 =>
    type TVal = Val0
    type TVar = Var0
  }

  trait Var[W, Val0 <: DFAny, Var0 <: Val0 with DFAny.Var[W, Val0, Var0]] extends DFAny.Val[W, Val0, Var0] {
    this : Val0 with Var0 =>
    type TAlias = TVar
    type TBool = DFBool.Var//DFBool#TVar
    type TBits[W2] = DFBits.Var[W2]//DFBits[W2]#TVar
    //    type TUInt = DFUInt#TVar

    final def dontProduce() : TAlias = {
      ???
      this.asInstanceOf[TAlias]
    }
    final def isNotFull : DFBool = ???
    final def := (that : TVal) : TVar = assign(that)
    final def assignNext(step : Int, that : TVal) : Unit = ???
    final def assignNext(step : Int, that : BigInt) : Unit = ???
    final def <-- (that : Iterable[TVal]) : TVar = {
      that.zipWithIndex.foreach{case (e, i) => this.assignNext(i, e)}
      this
    }
  }

  abstract class Alias(aliasedVar : DFAny, relWidth : Int, relBitLow : Int, deltaStep : Int = 0, initMask : InitMask = EmptyMask)
    extends DFAny {
    override protected[DFiant] lazy val almanacEntry : AlmanacEntry =
      AlmanacEntryAliasDFVar(aliasedVar.almanacEntry, BitsRange(relBitLow + relWidth - 1, relBitLow))
  }

  object Const {
    abstract class Int[C](constVal : TwoFace.Int[C]) extends DFAny {
//      implicit val widthOf: BitsWidthOf.Int[C]
//      val width = widthOf(constVal)
      override protected[DFiant] lazy val almanacEntry : AlmanacEntry =  AlmanacEntryConst(constVal.getValue)
    }
  }

  abstract class Op[W](val width : TwoFace.Int[W], almanacEntryOp: AlmanacEntryOp) extends DFAnyW[W] {
    override protected[DFiant] lazy val almanacEntry : AlmanacEntry = almanacEntryOp
  }
}




