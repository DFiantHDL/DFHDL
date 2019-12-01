package ZFiant

import singleton.ops._
import singleton.twoface._
import DFiant.internals.Meta

trait DFType {
  type TToken <: DFAny.Token
  type Width
  val width : TwoFace.Int[Width]
}
object DFType {
  implicit def ev[T <: DFAny](t : T) : t.TType = t.dfType
}

trait DFAny extends DFMember {
  type TType <: DFType
  type TVar
  val dfType : TType
  final protected val left : this.type = this
  protected type AsVal = DFAny.Aux[TType, false]
  protected type AsVar = DFAny.Aux[TType, true]
  protected type AsType[T <: DFType] = DFAny.Aux[T, TVar]
  final def bits(implicit ctx : DFAny.Context) : AsType[DFBits[dfType.Width]] =
    DFAny.Alias.BitsWL[dfType.Width, 0, this.type](left, dfType.width, 0)
  final def as[AT <: DFType](aliasType : AT)(implicit ctx : DFAny.Context) : AsType[AT] =
    DFAny.Alias.AsIs[AT, this.type](aliasType, left)
  final def prev(implicit ctx : DFAny.Context) : AsVal = DFAny.Alias.Prev[this.type](left, 1)
}

object DFAny {
  trait Context extends DFMember.Context
  protected[ZFiant] type Aux[Type <: DFType, Var] = DFAny {
    type TType = Type
    type TVar = Var
  }

  trait `Op:=`[To <: DFAny, From] {
    def apply(left : To, right : From) : Unit = {}
  }

  implicit class VarOps[L <: DFAny.Var[_ <: DFType]](left : L) {
    def := [R](right : R)(implicit op : `Op:=`[L, R]) : Unit = op(left, right)
  }

  trait Token

  trait ValOrVar[Type <: DFType, Var] extends DFAny {
    type TType = Type
    type TVar = Var
  }

  type Val[Type <: DFType] = ValOrVar[Type, false]
  type Var[Type <: DFType] = ValOrVar[Type, true]

  abstract class Constructor[Type <: DFType, Var] extends ValOrVar[Type, Var] {
    val ctx : DFAny.Context
  }

  final case class Const[Type <: DFType](dfType : Type, token : Type#TToken)(
    implicit val ctx : DFAny.Context
  ) extends Constructor[Type, false]

  sealed trait Initializable[Type <: DFType, Var] extends Constructor[Type, Var] {
    val externalInit : Seq[TType#TToken]
  }

  sealed trait Port[Type <: DFType, Var] extends Initializable[Type, Var] {
    val dir : Port.Dir
  }
  object Port {
    sealed trait Dir
    object Dir {
      case object IN extends Dir
      case object OUT extends Dir
    }
    final case class In[Type <: DFType](dfType : Type, externalInit : Seq[Type#TToken])(
      implicit val ctx : DFAny.Context
    ) extends Port[Type, false] {
      val dir : Port.Dir = Dir.IN
    }
    final case class Out[Type <: DFType](dfType : Type, externalInit : Seq[Type#TToken])(
      implicit val ctx : DFAny.Context
    ) extends Port[Type, true] {
      val dir : Port.Dir = Dir.OUT
    }
  }

  final case class NewVar[Type <: DFType](dfType : Type, externalInit : Seq[Type#TToken])(
    implicit val ctx : DFAny.Context
  ) extends Initializable[Type, true] {
    def ifdf(cond : ValOrVar[DFBool, _])(block : => ValOrVar[Type, _])(implicit ctx : DFBlock.Context)
    : ConditionalBlock.WithRetVal.IfBlock[Type] = ConditionalBlock.WithRetVal.IfBlock[Type](dfType, cond, () => block)
    override def toString: String = dfType.toString
  }

  trait Alias[Type <: DFType, RefVal <: DFAny, Var] extends Constructor[Type, Var] {
    val refVal : RefVal
  }
  object Alias {
    final case class AsIs[Type <: DFType, RefVal <: DFAny](dfType : Type, refVal : RefVal)(
      implicit val ctx : DFAny.Context
    ) extends Alias[Type, RefVal, RefVal#TVar]
    final case class BitsWL[W, L, RefVal <: DFAny](refVal : RefVal, relWidth : TwoFace.Int[W], relBitLow : TwoFace.Int[L])(
      implicit val ctx : DFAny.Context
    ) extends Alias[DFBits[W], RefVal, RefVal#TVar]{
      val dfType : TType = DFBits.dfType(relWidth)
    }
    final case class Prev[RefVal <: DFAny](refVal : RefVal, step : Int)(
      implicit val ctx : DFAny.Context
    ) extends Alias[RefVal#TType, RefVal, false] {
      val dfType : TType = refVal.dfType
    }
  }

  sealed abstract class Func[Type <: DFType] extends Constructor[Type, false]
  final case class Func2[Type <: DFType, L <: DFAny, R <: DFAny](dfType: Type, leftArg : L, rightArg : R)(
    implicit val ctx : DFAny.Context
  ) extends Func[Type]




  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Op
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  object Op {
    trait Able[R]{val value : R}
    object Able {
      implicit def fromAble[R](able : Able[R]) : R = able.value
    }
    trait Builder[L, R] {
      type Comp <: DFType
      def apply(left : L, rightR : R) : DFAny.Val[Comp]
    }
    type Context = DFBlock.Context
  }
  type `Op==Builder`[L, R] = Op.Builder[L, R]{type Comp = DFBool}
  type `Op!=Builder`[L, R] = Op.Builder[L, R]{type Comp = DFBool}
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


}



object Test {
  trait BB extends DFBlock {
    val a = DFUInt(8)
    DFUInt(8).ifdf(???) {
      a
    }.elsedf {
      a
    }
  }
//  val aa = a.bits.as(DFUInt(8)).bits

//  a := a
//  implicitly[aa.Var =:= true]
}