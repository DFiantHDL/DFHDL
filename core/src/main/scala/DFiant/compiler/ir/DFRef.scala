package DFiant.compiler.ir
import scala.annotation.unchecked.uncheckedVariance
import scala.reflect.{ClassTag, classTag}

type DFRefAny = DFRef[DFMember]
sealed trait DFRef[+M <: DFMember] derives CanEqual:
  lazy val refType: ClassTag[M @uncheckedVariance]
  final def =~(that: DFRefAny)(using MemberGetSet): Boolean =
    this.get =~ that.get
  final def get(using getSet: MemberGetSet): M = getSet(this)
object DFRef:
  trait OneWay[+M <: DFMember] extends DFRef[M]
  trait TwoWay[+M <: DFMember] extends DFRef[M]:
    lazy val originRef: OneWay[DFMember]
