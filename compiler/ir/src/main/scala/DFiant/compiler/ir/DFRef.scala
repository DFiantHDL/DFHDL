package DFiant.compiler.ir
import scala.annotation.unchecked.uncheckedVariance
import scala.reflect.{ClassTag, classTag}

type DFRefAny = DFRef[DFMember]
sealed trait DFRef[+M <: DFMember] derives CanEqual:
  lazy val refType: ClassTag[M @uncheckedVariance]
  final def =~(that: DFRefAny)(using MemberGetSet): Boolean = this.get =~ that.get
  def get(using getSet: MemberGetSet): M = getSet(this)
object DFRef:
  sealed trait Empty extends DFRef[DFMember.Empty]:
    lazy val refType = classTag[DFMember.Empty]
    override def get(using getSet: MemberGetSet): DFMember.Empty = DFMember.Empty
  trait OneWay[+M <: DFMember] extends DFRef[M]
  object OneWay:
    object Empty extends OneWay[DFMember.Empty] with DFRef.Empty

  trait TwoWay[+M <: DFMember] extends DFRef[M]:
    lazy val originRef: OneWay[DFMember]
  type TwoWayAny = TwoWay[DFMember]
  object TwoWay:
    object Empty extends TwoWay[DFMember.Empty] with DFRef.Empty:
      lazy val originRef: OneWay[DFMember] = OneWay.Empty
end DFRef
