package dfhdl.compiler.ir

enum DomainType extends HasRefCompare[DomainType] derives CanEqual:
  // dataflow domain
  case DF
  // register-transfer domain
  case RT(cfg: RTDomainCfg)
  // event-driven domain
  case ED

  protected def `prot_=~`(that: DomainType)(using MemberGetSet): Boolean =
    (this, that) match
      case (RT(l), RT(r)) => l =~ r
      case _              => this == that

  def getRefs: List[DFRef.TwoWayAny] = this match
    case RT(cfg) => cfg.getRefs
    case _       => Nil
end DomainType
