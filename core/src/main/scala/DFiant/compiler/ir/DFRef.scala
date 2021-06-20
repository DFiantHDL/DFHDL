package DFiant.compiler.ir
import scala.reflect.{ClassTag, classTag}

sealed trait DFRef:
  type Member <: DFMember
  val refType: ClassTag[Member]
  final def =~(that: DFRef)(using MemberGetSet): Boolean = this.get =~ that.get
  def get(using MemberGetSet): Member
object DFRef:
  trait OneWay[M <: DFMember] extends DFRef:
    type Member = M
  trait TwoWay[M <: DFMember] extends DFRef:
    type Member = M
    lazy val originRef: OneWay[DFMember]
