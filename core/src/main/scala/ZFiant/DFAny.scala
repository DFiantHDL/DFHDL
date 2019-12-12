package ZFiant

import singleton.ops._
import singleton.twoface._
import DFiant.internals._

import scala.annotation.implicitNotFound

sealed trait DFAny extends DFMember with Product with Serializable {
  type TType <: DFAny.Type
  type TMod <: DFAny.Modifier
  val dfType : TType
  val modifier : TMod
  type Width = dfType.Width
  type TToken = dfType.TToken
  final lazy val width = dfType.width
  final protected val left : this.type = this
  protected type AsVal = DFAny.ValOf[TType]
  protected type AsVar = DFAny.VarOf[TType]
  protected type AsType[T <: DFAny.Type] = DFAny.Value[T, TMod]
  protected type This = DFAny.Of[TType]
}

object DFAny {
  trait Type extends Product with Serializable {
    type TToken <: DFAny.Token
    type Width
    val width : TwoFace.Int[Width]
    type TPattern <: DFAny.Pattern[TPattern]
    type TPatternAble[+R] <: DFAny.Pattern.Able[R]
    type TPatternBuilder[L <: DFAny] <: DFAny.Pattern.Builder[L, TPatternAble]
    type OpAble[R] <: DFAny.Op.Able[R]
    type `Op==Builder`[-L, -R] <: DFAny.`Op==`.Builder[L, R]
    type `Op!=Builder`[-L, -R] <: DFAny.`Op!=`.Builder[L, R]
    type `Op<>Builder`[LType <: Type, -R] <: DFAny.`Op<>`.Builder[LType, R]
    type `Op:=Builder`[LType <: Type, -R] <: DFAny.`Op:=`.Builder[LType, R]
    type InitAble[L <: DFAny] <: DFAny.Init.Able[L]
    type InitBuilder[L <: DFAny] <: DFAny.Init.Builder[L, InitAble, TToken]
  }
  object Type {
    implicit def ev[T <: DFAny](t : T) : t.TType = t.dfType
  }

  @implicitNotFound(Context.MissingError.msg)
  final case class Context(meta : Meta, owner : DFBlock, compiler: DFCompiler) extends DFMember.Context
  object Context {
    final object MissingError extends ErrorMsg (
      "Missing an implicit owner Context.",
      "missing-context"
    ) {final val msg = getMsg}
  }

  trait Of[Type <: DFAny.Type] extends DFAny {
    type TType = Type
    //////////////////////////////////////////////////////////////////////////
    // Bit range selection
    //////////////////////////////////////////////////////////////////////////
    final def bits(implicit ctx : DFAny.Context) : AsType[DFBits.Type[dfType.Width]] =
      DFAny.Alias.BitsWL(this, dfType.width, 0)

    final protected def protBits[H, L](relBitHigh : TwoFace.Int[H], relBitLow : TwoFace.Int[L])(
      implicit relWidth : RelWidth.TF[H, L], ctx : DFAny.Context
    ) : AsType[DFBits.Type[relWidth.Out]] =
      DFAny.Alias.BitsWL(this, relWidth(relBitHigh, relBitLow), relBitLow)

    final def bits[H, L](relBitHigh : BitIndex.Checked[H, Width], relBitLow : BitIndex.Checked[L, Width])(
      implicit checkHiLow : BitsHiLo.CheckedShell[H, L], relWidth : RelWidth.TF[H, L], ctx : DFAny.Context
    ) : AsType[DFBits.Type[relWidth.Out]] = {
      checkHiLow.unsafeCheck(relBitHigh, relBitLow)
      protBits(relBitHigh.unsafeCheck(width), relBitLow.unsafeCheck(width))
    }

    final protected def protBitsWL[W, L](relWidth : TwoFace.Int[W], relBitLow : TwoFace.Int[L])(
      implicit ctx : DFAny.Context
    ) : AsType[DFBits.Type[W]] = DFAny.Alias.BitsWL(this, relWidth, relBitLow)

    final def bitsWL[W, L](relWidth : TwoFace.Int[W], relBitLow : BitIndex.Checked[L, Width])(
      implicit checkRelWidth : PartWidth.CheckedShell[W, Width - L], ctx : DFAny.Context
    ) : AsType[DFBits.Type[W]] = {
      checkRelWidth.unsafeCheck(relWidth, width-relBitLow)
      protBitsWL(relWidth, relBitLow.unsafeCheck(width))
    }
    //////////////////////////////////////////////////////////////////////////

    //////////////////////////////////////////////////////////////////////////
    // Prev
    //////////////////////////////////////////////////////////////////////////
    final protected def protPrev(step : Int)(implicit ctx : DFAny.Context)
    : AsVal = DFAny.Alias.Prev(this, step)
    final def prev()(implicit ctx : DFAny.Context) : AsVal = protPrev(1)
    final def prev[P](step : Natural.Int.Checked[P])(implicit ctx : DFAny.Context) : AsVal = protPrev(step)
    //////////////////////////////////////////////////////////////////////////

    //////////////////////////////////////////////////////////////////////////
    // Casting/Aliasing
    //////////////////////////////////////////////////////////////////////////
    final def as[AT <: DFAny.Type](aliasType : AT)(implicit ctx : DFAny.Context) : AsType[AT] =
      DFAny.Alias.AsIs(aliasType, this)
    //////////////////////////////////////////////////////////////////////////

    //////////////////////////////////////////////////////////////////////////
    // Equality
    //////////////////////////////////////////////////////////////////////////
    final def == [R](right : R)(
      implicit ccs: CaseClassSkipper[dfType.`Op==Builder`[This, R]]
    ) = ccs(op => op(left, right), left.asInstanceOf[Any] == right.asInstanceOf[Any])
    final def != [R](right : R)(
      implicit ccs: CaseClassSkipper[dfType.`Op!=Builder`[This, R]]
    ) = ccs(op => op(left, right), left.asInstanceOf[Any] != right.asInstanceOf[Any])
    //////////////////////////////////////////////////////////////////////////
  }

  trait Value[Type <: DFAny.Type, +Mod <: Modifier] extends DFAny.Of[Type] {
    type TMod <: Mod
  }
  implicit class PortOps1[Type <: DFAny.Type](left : PortOf[Type]) {
    def <>[R](right: left.dfType.OpAble[R])(
      implicit ctx: DFNet.Context, op: left.dfType.`Op<>Builder`[Type, R]
    ): Unit = left.connectWith(op(left.dfType, right))
  }
  implicit class PortOps2[L](left : L) {
    def <>[Type <: DFAny.Type](right: PortOf[Type])(
      implicit ctx: DFNet.Context, op: right.dfType.`Op<>Builder`[Type, L]
    ): Unit = right.connectWith(op(right.dfType, left))
  }

  sealed trait Modifier extends Product with Serializable
  object Modifier {
    sealed trait Val extends Modifier
    case object Val extends Val
    sealed trait Assignable extends Modifier
    sealed trait Connectable extends Modifier
    sealed trait Initializable extends Modifier
    sealed trait Initialized[Token <: DFAny.Token] extends Modifier {
      val externalInit : Seq[Token]
    }
    final case class Constant[Token <: DFAny.Token](token : Token) extends Initialized[Token] with Val {
      override val externalInit: Seq[Token] = Seq(token)
    }
    sealed trait Port extends Connectable
    object Port {
      sealed trait In extends Port
      sealed trait Out extends Port with Assignable
    }
    sealed trait NewVar extends Connectable with Assignable
  }

  final case class Const[Type <: DFAny.Type](dfType : Type, token : Type#TToken, ownerRef: DFRef[DFBlock], meta: Meta) extends Value[Type, Modifier.Constant[Type#TToken]] {
    type TMod = Modifier.Constant[Type#TToken]
    val modifier : TMod = Modifier.Constant(token)
  }
  object Const {
    def apply[Type <: DFAny.Type](dfType: Type, token: Type#TToken)(implicit ctx: Context)
    : Const[Type] = ctx.compiler.addMember(Const[Type](dfType, token, ctx.owner, ctx.meta))
  }

  object Port {
    final case class In[Type <: DFAny.Type, Mod <: DFAny.Modifier.Port.In](
      dfType : Type, modifier : Mod, ownerRef: DFRef[DFBlock], meta: Meta
    ) extends Value[Type, Mod] {
      type TMod = Mod
    }
    object In {
      sealed trait Uninitialized extends DFAny.Modifier.Port.In with DFAny.Modifier.Initializable
      case object Uninitialized extends Uninitialized
      case class Initialized[Token <: DFAny.Token](externalInit: Seq[Token]) extends DFAny.Modifier.Port.In with DFAny.Modifier.Initialized[Token]
      implicit class InitializableOps[Type <: DFAny.Type](val i : In[Type, Uninitialized]) {
        def init(that : i.dfType.InitAble[i.This]*)(
          implicit op : i.dfType.InitBuilder[i.This], ctx : DFAny.Context
        ) : In[Type, Initialized[i.TToken]] =
          ctx.compiler.addMember(In[Type, Initialized[i.TToken]](i.dfType, Initialized(op(i, that)), ctx.owner, ctx.meta))
      }
      def apply[Type <: DFAny.Type](dfType: Type)(
        implicit ctx: DFAny.Context
      ): In[Type, Uninitialized] = ctx.compiler.addMember(In[Type, Uninitialized](dfType, Uninitialized, ctx.owner, ctx.meta))
    }
    final case class Out[Type <: DFAny.Type, Mod <: DFAny.Modifier.Port.Out](
      dfType : Type, modifier : Mod, ownerRef: DFRef[DFBlock], meta: Meta
    ) extends Value[Type, Mod] {
      type TMod = Mod
    }
    object Out {
      sealed trait Uninitialized extends DFAny.Modifier.Port.Out with DFAny.Modifier.Initializable
      case object Uninitialized extends Uninitialized
      case class Initialized[Token <: DFAny.Token](externalInit: Seq[Token]) extends DFAny.Modifier.Port.Out with DFAny.Modifier.Initialized[Token]
      implicit class InitializableOps[Type <: DFAny.Type](val i : Out[Type, Uninitialized]) {
        def init(that : i.dfType.InitAble[i.This]*)(
          implicit op : i.dfType.InitBuilder[i.This], ctx : DFAny.Context
        ) : Out[Type, Initialized[i.TToken]] =
          ctx.compiler.addMember(Out[Type, Initialized[i.TToken]](i.dfType, Initialized(op(i, that)), ctx.owner, ctx.meta))
      }
      def apply[Type <: DFAny.Type](dfType: Type)(
        implicit ctx: DFAny.Context
      ): Out[Type, Uninitialized] = ctx.compiler.addMember(Out[Type, Uninitialized](dfType, Uninitialized, ctx.owner, ctx.meta))
    }
  }

  final case class NewVar[Type <: DFAny.Type, Mod <: DFAny.Modifier.NewVar](
    dfType : Type, modifier : Mod, ownerRef: DFRef[DFBlock], meta: Meta
  ) extends Value[Type, Mod] {
    type TMod = Mod
    def <> (in : IN)(implicit ctx : DFAny.Context) : Port.In[Type, Port.In.Uninitialized] = Port.In(dfType)
    def <> (out : OUT)(implicit ctx : DFAny.Context) : Port.Out[Type, Port.Out.Uninitialized] = Port.Out(dfType)
    def <>[R](right: DFAny.PortOf[Type])(
      implicit ctx: DFNet.Context, op: dfType.`Op<>Builder`[Type, DFAny.PortOf[Type]]
    ): Unit = left.connectWith(op(dfType, right))
    def ifdf[C, B](cond : DFBool.Op.Able[C])(block : => dfType.OpAble[B])(
      implicit ctx : DFBlock.Context, condConv : DFBool.`Op:=`.Builder[DFBool.Type, C], blockConv : dfType.`Op:=Builder`[Type, B]
    ) : ConditionalBlock.WithRetVal.IfBlock[Type] = ConditionalBlock.WithRetVal.IfBlock[Type](
      dfType, condConv(DFBool.Type(),cond)
    )(blockConv(dfType, block))(ctx)
    def matchdf[MVType <: DFAny.Type](matchValue : DFAny.Of[MVType], matchConfig : MatchConfig = MatchConfig.NoOverlappingCases)(
      implicit ctx : DFBlock.Context
    ): ConditionalBlock.WithRetVal.MatchHeader[Type, MVType] =
      ConditionalBlock.WithRetVal.MatchHeader[Type, MVType](dfType, matchValue, matchConfig)(ctx)
    override def toString: String = dfType.toString
  }
  object NewVar {
    sealed trait Uninitialized extends DFAny.Modifier.NewVar with DFAny.Modifier.Initializable
    case object Uninitialized extends Uninitialized
    case class Initialized[Token <: DFAny.Token](externalInit: Seq[Token]) extends DFAny.Modifier.NewVar with DFAny.Modifier.Initialized[Token]
    implicit class InitializableOps[Type <: DFAny.Type](val i : NewVar[Type, Uninitialized]) {
      def init(that : i.dfType.InitAble[i.This]*)(
        implicit op : i.dfType.InitBuilder[i.This], ctx : DFAny.Context
      ) : NewVar[Type, Initialized[i.TToken]] =
        ctx.compiler.addMember(NewVar[Type, Initialized[i.TToken]](i.dfType, Initialized(op(i, that)), ctx.owner, ctx.meta))
    }
    def apply[Type <: DFAny.Type](dfType: Type)(
      implicit ctx: Context
    ): NewVar[Type, Uninitialized] = ctx.compiler.addMember(NewVar[Type, Uninitialized](dfType, Uninitialized, ctx.owner, ctx.meta))
  }

  sealed trait Alias[Type <: DFAny.Type, RefVal <: DFAny, +Mod <: Modifier] extends Value[Type, Mod] {
    val retValRef : DFRef[RefVal]
  }
  object Alias {
    final case class AsIs[Type <: DFAny.Type, RefVal <: DFAny](
      dfType : Type, retValRef : DFRef[RefVal], ownerRef: DFRef[DFBlock], meta: Meta
    ) extends Alias[Type, RefVal, RefVal#TMod] {
      val retVal : RefVal = retValRef
      type TMod = retVal.TMod
      val modifier : TMod = retVal.modifier
    }
    object AsIs {
      def apply[Type <: DFAny.Type, RefVal <: DFAny](dfType: Type, refVal: RefVal)(
        implicit ctx: Context
      ): AsIs[Type, RefVal] = ctx.compiler.addMember(AsIs[Type, RefVal](dfType, refVal, ctx.owner, ctx.meta))
    }
    final case class BitsWL[W, L, RefVal <: DFAny](
      retValRef : DFRef[RefVal], relWidth : TwoFace.Int[W], relBitLow : TwoFace.Int[L], ownerRef: DFRef[DFBlock], meta: Meta
    ) extends Alias[DFBits.Type[W], RefVal, RefVal#TMod]{
      val retVal : RefVal = retValRef
      type TMod = retVal.TMod
      val dfType : TType = DFBits.Type(relWidth)
      val modifier : TMod = retVal.modifier
    }
    object BitsWL {
      def apply[W, L, RefVal <: DFAny](refVal: RefVal, relWidth: TwoFace.Int[W], relBitLow: TwoFace.Int[L])(
        implicit ctx: Context
      ): BitsWL[W, L, RefVal] = ctx.compiler.addMember(BitsWL(refVal, relWidth, relBitLow, ctx.owner, ctx.meta))
    }
    final case class Prev[RefVal <: DFAny](
      retValRef : DFRef[RefVal], step : Int, ownerRef: DFRef[DFBlock], meta: Meta
    ) extends Alias[RefVal#TType, RefVal, Modifier.Val] {
      type TMod = Modifier.Val
      val dfType : TType = retValRef.dfType
      val modifier : TMod = Modifier.Val
    }
    object Prev {
      def apply[RefVal <: DFAny](refVal: RefVal, step: Int)(
        implicit ctx: Context
      ): Prev[RefVal] = ctx.compiler.addMember(Prev[RefVal](refVal, step, ctx.owner, ctx.meta))
    }
  }

  sealed abstract class Func[Type <: DFAny.Type] extends Value[Type, Modifier.Val] {
    type TMod = Modifier.Val
    val modifier : TMod = Modifier.Val
  }
  final case class Func2[Type <: DFAny.Type, L <: DFAny, Op <: DiSoOp, R <: DFAny](
    dfType: Type, leftArg : DFRef[L], op : Op, rightArg : DFRef[R], ownerRef: DFRef[DFBlock], meta: Meta
  )(func : (L#TToken, R#TToken) => Type#TToken) extends Func[Type]
  object Func2 {
    def apply[Type <: DFAny.Type, L <: DFAny, Op <: DiSoOp, R <: DFAny](
      dfType: Type, leftArg: L, op: Op, rightArg: R
    )(func: (L#TToken, R#TToken) => Type#TToken)(implicit ctx: Context)
    : Func2[Type, L, Op, R] = ctx.compiler.addMember(Func2(dfType, leftArg, op, rightArg, ctx.owner, ctx.meta)(func))
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Extension Classes
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  type ValOf[Type <: DFAny.Type] = Value[Type, Modifier.Val]
  type VarOf[Type <: DFAny.Type] = Value[Type, Modifier.Assignable]
  implicit class VarOps[Type <: DFAny.Type](left : DFAny.VarOf[Type]) {
    def := [R](right : left.dfType.OpAble[R])(
      implicit ctx : DFNet.Context, op : left.dfType.`Op:=Builder`[Type, R]
    ) : Unit = DFNet.Assignment(left, op(left.dfType, right))
  }

  type ConnectableOf[Type <: DFAny.Type] = Value[Type, Modifier.Connectable]
  type PortOf[Type <: DFAny.Type] = Value[Type, Modifier.Port]
  type PortInOf[Type <: DFAny.Type] = Value[Type, Modifier.Port.In]
  type PortOutOf[Type <: DFAny.Type] = Value[Type, Modifier.Port.Out]

  implicit class ConnectableOps[Type <: DFAny.Type](left : ConnectableOf[Type]){
    protected type ConnRet = DFAny//ConnectableOf[_ <: DFAny.Type]
    protected type PortIn = DFAny//PortInOf[_ <: DFAny.Type]
    protected type PortOut = DFAny//PortOutOf[_ <: DFAny.Type]
    type This = DFAny//Of[_ <: DFAny.Type]

    protected implicit class ConnectionExtras(that : DFAny) {
      def isConnectingExternally(implicit ctx : DFNet.Context) : Boolean = that.ownerDesign.ownerDesign == ctx.owner
      def isConnectingInternally(implicit ctx : DFNet.Context) : Boolean = that.ownerDesign == ctx.owner
    }
    private def connectPortInWithPortIn(left : PortIn, right : PortIn)(implicit ctx : DFNet.Context) : (ConnRet, This) = {
      def throwConnectionError(msg : String) = throw new IllegalArgumentException(s"\n$msg\nAttempted connection: ${left.fullName} <> ${right.fullName} at ${ctx.owner.fullName}")
      if (left isSameOwnerDesignAs right) throwConnectionError("Cannot connect two input ports of the same design.")
      //Connecting owner and child design input ports, while child port is left and owner port is right.
      else if ((left isOneLevelBelow right) && (left.isConnectingExternally)) (left, right)
      //Connecting owner and child design input ports, while child port is right and owner port is left.
      else if ((right isOneLevelBelow left) && (right.isConnectingExternally)) (right, left)
      else throwConnectionError("Unsupported connection")
    }
    private def connectPortOutWithPortOut(left : PortOut, right : PortOut)(implicit ctx : DFNet.Context) : (ConnRet, This) = {
      def throwConnectionError(msg : String) = throw new IllegalArgumentException(s"\n$msg\nAttempted connection: ${left.fullName} <> ${right.fullName} at ${ctx.owner.fullName}")
      if (left isSameOwnerDesignAs right) throwConnectionError("Cannot connect two output ports of the same design.")
      //Connecting owner and child design output ports, while child port is left and owner port is right.
      else if ((left isOneLevelBelow right) && (left.isConnectingExternally)) (right, left)
      //Connecting owner and child design output ports, while child port is right and owner port is left.
      else if ((right isOneLevelBelow left) && (right.isConnectingExternally)) (left, right)
      else throwConnectionError("Unsupported connection")
    }
    private def connectPortOutWithPortIn(out : PortOut, in : PortIn)(implicit ctx : DFNet.Context) : (ConnRet, This) = {
      def throwConnectionError(msg : String) = throw new IllegalArgumentException(s"\n$msg\nAttempted connection: ${out.fullName} <> ${in.fullName} at ${ctx.owner.fullName}")
      //Connecting input and output ports internally at the same design
      if ((out isSameOwnerDesignAs in) && out.isConnectingInternally) (out, in)
      //Connecting input and output ports of sibling designs
      else if ((out.ownerDesign isSameOwnerDesignAs in.ownerDesign) && out.isConnectingExternally) (in, out)
      else throwConnectionError("Unsupported connection")
    }
    private def connectValWithPortIn(dfVal : This, in : PortIn)(implicit ctx : DFNet.Context) : (ConnRet, This) = {
//      //Connecting a value to an input port externally
//      if ((in isOneLevelBelow dfVal) && (in.isConnectingExternally)) (in, dfVal)
//      //Connecting owner and child design input ports, while child port is right and owner port is left.
//      else dfVal match {
//        case c : Connectable[Type, _] if ((right isOneLevelBelow left) && (right.isConnectingExternally)) (right, left) =>
//
//      }
//
//
//      else throwConnectionError("Unsupported connection")
      ???
    }
    private def connectValWithPortOut(dfVal : This, out : PortOut)(implicit ctx : DFNet.Context) : (ConnRet, This) = {
      ???

    }

    object In {
      def unapply[T <: DFAny.Type, M <: Modifier](arg: Value[T, M]): Boolean = arg.modifier match {
        case _ : Modifier.Port.In => true
        case _ => false
      }
    }
    object Out {
      def unapply[T <: DFAny.Type, M <: Modifier](arg: Value[T, M]): Boolean = arg.modifier match {
        case _ : Modifier.Port.Out => true
        case _ => false
      }
    }
    protected[ZFiant] def connectWith(right : This)(implicit ctx : DFNet.Context) : Unit = {
      def throwConnectionError(msg : String) = throw new IllegalArgumentException(s"\n$msg\nAttempted connection: ${left.fullName} <> ${right.fullName}")
      val (toPort, from) : (ConnRet, DFAny) = (left, right) match {
        case (p1@In(), p2@In()) => connectPortInWithPortIn(p1, p2)
        case (p1@Out(), p2@Out()) => connectPortOutWithPortOut(p1, p2)
        case (p1@Out(), p2@In()) => connectPortOutWithPortIn(p1, p2)
        case (p1@In(), p2@Out()) => connectPortOutWithPortIn(p2, p1)
        case (p@In(), v) => connectValWithPortIn(v, p)
        case (v, p@In()) => connectValWithPortIn(v, p)
        case (p@Out(), v) => connectValWithPortOut(v, p)
        case (v, p@Out()) => connectValWithPortOut(v, p)
        case _ => throwConnectionError(s"Connection must be made between a port and a value or between ports. No ports found.")
      }
      DFNet.Connection(toPort, from)
    }
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Token
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  trait Token extends Product with Serializable {
    type TValue
    type Width
    //maximum token value width
    val width : TwoFace.Int[Width]
    val value : TValue
    val bubbleMask : XBitVector[Width]
    val valueBits : XBitVector[Width]
    final def isBubble : Boolean = !(bubbleMask === XBitVector.low(width))
    final def bits : DFBits.Token[Width] = DFBits.Token(width, valueBits, bubbleMask)
    final def bit(relBit : Int) : DFBool.Token = {
      val outBitsValue = valueBits.bit(relBit)
      val outBubbleMask = bubbleMask.bit(relBit)
      DFBool.Token(outBitsValue, outBubbleMask)
    }
    final def bitsWL[W](relWidth : TwoFace.Int[W], relBitLow : Int) : DFBits.Token[W] = {
      val outBitsValue = valueBits.bitsWL(relWidth, relBitLow)
      val outBubbleMask = bubbleMask.bitsWL(relWidth, relBitLow)
      DFBits.Token(relWidth, outBitsValue, outBubbleMask)
    }
  }
  object Token {
    trait Of[Value, W] extends Token {
      type TValue = Value
      type Width = W
    }
    trait BubbleOfToken[T <: Token] {
      def apply(t : T) : T
    }
    trait BubbleOfDFType[Type <: DFAny.Type] {
      def apply(t : Type) : Type#TToken
    }
    implicit class TokenSeqInit[T <: Token](tokenSeq : Seq[T]) {
      def prevInit(step : Int) : Seq[T] = {
        val length = tokenSeq.length
        //No init at all, so invoking prev does not change anything (bubble tokens will be used)
        if ((length == 0) || (step == 0)) tokenSeq
        //The step is larger or equals to the init sequence, so only the last init token remains
        else if (length <= step) Seq(tokenSeq.last)
        //More tokens are available than the step size, so we drop the first, according to the step count
        else tokenSeq.drop(step)
      }
      def bits : Seq[DFBits.Token[T#Width]] =
        tokenSeq.map(t => t.bits.asInstanceOf[DFBits.Token[T#Width]])
      def bitsWL[W](relWidth : TwoFace.Int[W], relBitLow : Int) : Seq[DFBits.Token[W]] =
        tokenSeq.map(t => t.bitsWL(relWidth, relBitLow))
      //      def patternMatch(pattern : T#TPattern) : Seq[DFBool.Token] = TokenSeq(tokenSeq, pattern)((l, r) => l.patternMatch(r.asInstanceOf[l.TPattern]))
    }
  }
  //  trait Token {self =>
  //    type TValue
  //    protected[DFiant] type TToken <: Token
  //    protected[DFiant] type TPattern <: DFAny.Pattern[TPattern]{type TValue = self.TValue}
  //    val value : TValue
  //    val width : Int
  //    final lazy val widthOfValue : Int = scala.math.max(valueBits.lengthOfValue, bubbleMask.lengthOfValue).toInt
  //    val valueBits : BitVector
  //    val bubbleMask : BitVector
  //    //leading zero counter
  //    final lazy val lzc : Int = scala.math.min(valueBits.lzc, bubbleMask.lzc).toInt
  //    def toBubbleToken : Token
  //
  //    final def patternMatch(that : TPattern) : DFBool.Token = DFBool.Token(that.matches(this.value), this.isBubble)
  //  }
  //
  //  object Token {
  ////    trait Resizable extends Token {
  ////      def resize(toWidth : Int) : TToken
  ////    }
  ////    abstract class Of[V, P <: DFAny.Pattern[P]{type TValue = V}](implicit codeStringOf : CodeStringOf[V]) extends Token {
  ////      type TValue = V
  ////      protected[DFiant] type TPattern = P
  ////      final def codeString : String = if (isBubble) "Φ" else value.codeString
  ////    }
  ////    def patternMatch[T <: Token, P <: Pattern[_]](tokenSeq : Seq[T], pattern : P) : Seq[DFBool.Token] = TokenSeq(tokenSeq, pattern)((l, r) => l.patternMatch(r.asInstanceOf[l.TPattern]))
  //  }
  //
  object TokenSeq {
    def apply[O <: Token, T1 <: Token, T2 <: Token, T3 <: Token](t1 : Seq[T1], t2 : Seq[T2], t3 : Seq[T3])(op : (T1, T2, T3) => O) : Seq[O] =
      if (t1.isEmpty || t2.isEmpty || t3.isEmpty) Seq() else{
        val leftSeq = t1
        val rightSeq = t2
        val leftSeq2 = leftSeq.zipAll(rightSeq, leftSeq.last, rightSeq.last)
        val rightSeq2 = t3
        leftSeq2.zipAll(rightSeq2, leftSeq2.last, rightSeq2.last).map(t => op(t._1._1, t._1._2, t._2))
      }
    def apply[O <: Token, L <: Token, R <: Token](leftSeq : Seq[L], rightSeq : Seq[R])(op : (L, R) => O) : Seq[O] =
      if (leftSeq.isEmpty || rightSeq.isEmpty) Seq() else
        leftSeq.zipAll(rightSeq, leftSeq.last, rightSeq.last).map(t => op(t._1, t._2))
    def apply[O <: Token, L <: Token, R](leftSeq : Seq[L], rightConst : R)(op : (L, R) => O) : Seq[O] =
      leftSeq.map(t => op(t, rightConst))
    def apply[O <: Token, T <: Token](seq : Seq[T])(op : T => O) : Seq[O] =
      seq.map(t => op(t))
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Match Pattern
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  sealed trait Pattern[P <: Pattern[P]] {
    type TValue
    def matches(value : TValue) : Boolean
    def overlapsWith(pattern: P) : Boolean
    def codeString : String
    override def toString: String = codeString
  }
  object Pattern {
    abstract class OfIntervalSet[T, P <: OfIntervalSet[T, P]](val patternSet : IntervalSet[T])(implicit codeStringOf: CodeStringOf[Interval[T]]) extends Pattern[P] {
      type TValue = T
      def matches(value : TValue) : Boolean = patternSet.containsPoint(value)
      def overlapsWith(pattern: P) : Boolean = patternSet.intersect(pattern.patternSet).nonEmpty
      def codeString : String = patternSet.map(t => t.codeString).mkString(", ")
    }
    abstract class OfSet[T, P <: OfSet[T, P]](val patternSet : Set[T])(implicit codeStringOf: CodeStringOf[T]) extends Pattern[P] {
      type TValue = T
      def matches(value : TValue) : Boolean = patternSet.contains(value)
      def overlapsWith(pattern: P) : Boolean = patternSet.intersect(pattern.patternSet).nonEmpty
      def codeString : String = patternSet.map(t => t.codeString).mkString(", ")
    }
    trait Able[+R] {
      val right : R
    }

    trait Builder[L <: DFAny, Able[+R] <: Pattern.Able[R]] {
      def apply[R](left : L, right : Seq[Able[R]]) : left.dfType.TPattern
    }
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Init
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  object Init {
    trait Able[L <: DFAny] {
      val right : Any

      override def toString: String = right.toString
    }
    object Able {
      implicit class AbleSeq[L <: DFAny](s : Seq[Able[L]]) {
        private def flatten(s: Seq[Any]): Seq[Any] = s flatMap {
          case ss: Seq[_] => flatten(ss)
          case e => Seq(e)
        }
        def toSeqAny : Seq[Any] = {
          flatten(s.map(e => e.right))
        }

        override def toString: String = s.toString()
      }
    }
    trait Builder[L <: DFAny, Able[L0 <: DFAny] <: Init.Able[L0], Token <: DFAny.Token] {
      def apply(left : L, right : Seq[Able[L]]) : Seq[Token]
    }
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Op
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  object Op {
    trait Able[R]{val value : R}
    object Able {
      implicit def fromAble[R](able : Able[R]) : R = able.value
    }
    trait Builder[-L, -R] extends HasOut {
      type Out <: DFAny
      def apply(left : L, rightR : R) : Out
    }
  }
  object `Op==` {
    type Builder[-L, -R] = Op.Builder[L, R]{type Out = DFBool}
  }
  object `Op!=` {
    type Builder[-L, -R] = Op.Builder[L, R]{type Out = DFBool}
  }
  object `Op<>` {
    type Builder[LType <: Type, -R] = Op.Builder[LType, R]{type Out = Of[LType]}
  }
  object `Op:=` {
    type Builder[LType <: Type, -R] = Op.Builder[LType, R]{type Out = Of[LType]}
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Create Companion object of DFXXX extenders of DFAny
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  trait Companion {
    /////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Init
    /////////////////////////////////////////////////////////////////////////////////////////////////////////////
    trait InitCO {
      type Able[L <: DFAny] <: DFAny.Init.Able[L]
      type Builder[L <: DFAny, Token <: DFAny.Token] <: DFAny.Init.Builder[L, Able, Token]
    }
    val Init : InitCO
    /////////////////////////////////////////////////////////////////////////////////////////////////////////////

    /////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Match Pattern
    /////////////////////////////////////////////////////////////////////////////////////////////////////////////
    trait PatternCO {
      type Able[+R] <: DFAny.Pattern.Able[R]
      type Builder[L <: DFAny] <: DFAny.Pattern.Builder[L, Able]
    }
    val Pattern : PatternCO
    /////////////////////////////////////////////////////////////////////////////////////////////////////////////

    /////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // General Op
    /////////////////////////////////////////////////////////////////////////////////////////////////////////////
    trait OpCO {
      type Able[R] <: DFAny.Op.Able[R]
      type Implicits
      val Able : Implicits
    }
    val Op : OpCO
    /////////////////////////////////////////////////////////////////////////////////////////////////////////////

    /////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Common Ops
    /////////////////////////////////////////////////////////////////////////////////////////////////////////////
    trait `Op==` {
      type Builder[-L, -R] <: DFAny.`Op==`.Builder[L, R]
    }
    val `Op==` : `Op==`
    trait `Op!=` {
      type Builder[-L, -R] <: DFAny.`Op!=`.Builder[L, R]
    }
    val `Op!=` : `Op!=`
    trait `Op<>` {
      type Builder[LType <: Type, -R] <: DFAny.`Op<>`.Builder[LType, R]
    }
    val `Op<>` : `Op<>`
    trait `Op:=` {
      type Builder[LType <: Type, -R] <: DFAny.`Op:=`.Builder[LType, R]
    }
    val `Op:=` : `Op:=`
    /////////////////////////////////////////////////////////////////////////////////////////////////////////////
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
}
