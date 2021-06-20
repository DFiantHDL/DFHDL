package DFiant.compiler.ir
import scala.reflect.{ClassTag, classTag}

sealed trait DFRef:
  type Member <: DFMember
  val refType: ClassTag[Member]
  final def =~(that: DFRef)(using MemberGetSet): Boolean = this.get =~ that.get
  final def get(using getSet: MemberGetSet): Member = getSet(this)
object DFRef:
  type Of[M <: DFMember] = DFRef { type Member = M }
  trait OneWay[M <: DFMember] extends DFRef:
    type Member = M
  object Empty extends OneWay[Nothing]:
    val refType = ???
  trait TwoWay[M <: DFMember] extends DFRef:
    type Member = M
    lazy val originRef: OneWay[DFMember]
