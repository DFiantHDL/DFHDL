package dfhdl.compiler.ir
import dfhdl.internals.*
import dfhdl.compiler.analysis.*
import dfhdl.compiler.ir.constraints.Timing

enum DomainPoint derives CanEqual:
  case Via(designInst: DFDesignInst, domainNamePath: String) extends DomainPoint
  case Direct(domain: DFDomainOwner) extends DomainPoint
object DomainPoint:
  object Via:
    def apply(designInst: DFDesignInst, domain: DFDomainOwner)(using
        MemberGetSet
    ): DomainPoint.Via =
      DomainPoint.Via(
        designInst,
        domain.getRelativeName(designInst.getDesignBlock)
      )
  end Via
end DomainPoint

//                                key depends on value
type DependentRTDomainMap = Map[DomainPoint, DomainPoint]
