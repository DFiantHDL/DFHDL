package ZFiant

import singleton.ops._
import singleton.twoface._
trait DFAnyMember
trait DFType {
  type TToken <: DFAny.Token
  type Width
  val width : TwoFace.Int[Width]
}
object DFType {
  implicit def ev[T <: DFAny](t : T) : t.Type = t.dfType
}

trait DFAny extends DFAnyMember {
  type Type <: DFType
  type Var
  val dfType : Type
  final protected val left : this.type = this
  protected type AsVal = DFAny.Aux[Type, false]
  protected type AsVar = DFAny.Aux[Type, true]
  protected type AsType[T <: DFType] = DFAny.Aux[T, Var]
  final def bits : AsType[DFBits[dfType.Width]] = DFAny.Alias.BitsWL[dfType.Width, 0, this.type](left)(dfType.width, 0)
  final def as[AT <: DFType](aliasType : AT) : AsType[AT] = DFAny.Alias.AsIs[AT, this.type](aliasType)(left)
  final def prev : AsVal = DFAny.Alias.Prev[this.type](left)(1)
}

object DFAny {
  protected type Aux[Type0 <: DFType, Var0] = DFAny {
    type Type = Type0
    type Var = Var0
  }

  trait `Op:=`[To <: DFAny, From] {
    def apply(left : To, right : From) : Unit = {}
  }

  implicit class VarOps[L <: DFAny](left : L)(implicit isVar : L#Var =:= true) {
    def := [R](right : R)(implicit op : `Op:=`[L, R]) : Unit = op(left, right)
  }

  trait Token

  sealed abstract class Constructor[Type0 <: DFType, Var0](val dfType : Type0) extends DFAny {
    type Type = Type0
    type Var = Var0
  }

  case class Const[Type <: DFType](override val dfType : Type)(val token : Type#TToken) extends Constructor[Type, false](dfType)

  sealed abstract class Initializable[Type <: DFType, Var](override val dfType : Type)(val externalInit : Seq[Type#TToken]) extends Constructor[Type, Var](dfType)

  sealed abstract class Port[Type <: DFType, Var](override val dfType : Type)(val dir : Port.Dir, externalInit : Seq[Type#TToken]) extends Initializable[Type, Var](dfType)(externalInit)
  object Port {
    sealed trait Dir
    object Dir {
      case object IN extends Dir
      case object OUT extends Dir
    }
    case class In[Type <: DFType](override val dfType : Type)(externalInit : Seq[Type#TToken]) extends Port[Type, false](dfType)(Dir.IN, externalInit)
    case class Out[Type <: DFType](override val dfType : Type)(externalInit : Seq[Type#TToken]) extends Port[Type, true](dfType)(Dir.OUT, externalInit)
  }

  case class NewVar[Type <: DFType](override val dfType : Type)(externalInit : Seq[Type#TToken]) extends Initializable[Type, true](dfType)(externalInit)

  sealed abstract class Alias[Type <: DFType, RefVal <: DFAny, Var](override val dfType : Type)(val refVal : RefVal) extends Constructor[Type, Var](dfType)
  object Alias {
    case class AsIs[Type <: DFType, RefVal <: DFAny](override val dfType : Type)(refVal : RefVal) extends Alias[Type, RefVal, RefVal#Var](dfType)(refVal)
    case class BitsWL[W, L, RefVal <: DFAny](override val refVal : RefVal)(val relWidth : TwoFace.Int[W], val relBitLow : TwoFace.Int[L]) extends Alias[DFBits[W], RefVal, RefVal#Var](DFBits.dfType(relWidth))(refVal)
    case class Prev[RefVal <: DFAny](override val refVal : RefVal)(val step : Int) extends Alias[RefVal#Type, RefVal, false](refVal.dfType)(refVal)
  }

  sealed abstract class Func[OutType <: DFType](outType : OutType) extends Constructor[OutType, false](outType)
  case class Func2[OutType <: DFType, L <: DFAny, R <: DFAny](outType: OutType)(left : L, right : R) extends Func(outType)


}



object Test {
  val a = DFUInt(8)
  val aa = a.bits.as(DFUInt(8)).bits
//  a := a
//  implicitly[aa.Var =:= true]
}