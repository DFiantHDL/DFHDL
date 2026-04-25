package dfhdl.compiler.ir
import upickle.default.*

enum DomainType extends HasRefCompare[DomainType] derives CanEqual, ReadWriter:
  // dataflow domain
  case DF
  // register-transfer domain
  case RT
  // event-driven domain
  case ED

  protected def `prot_=~`(that: DomainType)(using MemberGetSet): Boolean =
    this == that

  lazy val getRefs: List[DFRef.TwoWayAny] = Nil

  def copyWithNewRefs(using RefGen): this.type = this

end DomainType
