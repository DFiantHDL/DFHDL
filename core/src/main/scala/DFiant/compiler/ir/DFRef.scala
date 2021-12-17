package DFiant.compiler.ir
import scala.annotation.unchecked.uncheckedVariance
import scala.reflect.{ClassTag, classTag}

type DFRefAny = DFRef[DFMember]
sealed trait DFRef[+M <: DFMember] derives CanEqual:
  lazy val refType: ClassTag[M @uncheckedVariance]
  final def =~(that: DFRefAny)(using MemberGetSet): Boolean =
    (this, that) match
      case (_: DFRef.Empty, _: DFRef.Empty) => true
      case (_: DFRef.Empty, _)              => false
      case (_, _: DFRef.Empty)              => false
      case _                                => this.get =~ that.get
  final def get(using getSet: MemberGetSet): M = getSet(this)
  final def isEmpty: Boolean =
    this match
      case _: DFRef.Empty => true
      case _              => false
object DFRef:
  sealed trait Empty extends DFRef[Nothing]:
    lazy val refType = throw new IllegalArgumentException(
      "Illegal access to an empty ref"
    )
  trait OneWay[+M <: DFMember] extends DFRef[M]
  object OneWay:
    object Empty extends OneWay[Nothing] with DFRef.Empty

  trait TwoWay[+M <: DFMember] extends DFRef[M]:
    lazy val originRef: OneWay[DFMember]
  object TwoWay:
    object Empty extends TwoWay[Nothing] with DFRef.Empty:
      lazy val originRef: OneWay[DFMember] = OneWay.Empty
