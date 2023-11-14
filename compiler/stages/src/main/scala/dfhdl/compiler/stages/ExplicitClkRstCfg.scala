package dfhdl.compiler.stages

import dfhdl.compiler.analysis.*
import dfhdl.compiler.ir.*
import dfhdl.compiler.patching.*
import dfhdl.options.CompilerOptions
import scala.annotation.tailrec

/** This stage converts a derived clock-reset configuration of RT designs/domains to explicit
  * configurations.
  */
case object ExplicitClkRstCfg extends Stage:
  def dependencies: List[Stage] = List()
  def nullifies: Set[Stage] = Set()
  def transform(designDB: DB)(using getSet: MemberGetSet, co: CompilerOptions): DB =
    val patchList: List[(DFMember, Patch)] = designDB.namedOwnerMemberList.flatMap {
      case (owner: (DFDomainOwner & DFBlock), members) =>
        owner.domainType match
          case domainType @ DomainType.RT(_: DerivedCfg.type) =>
            @tailrec def getExplicitCfg(currentOwner: DFDomainOwner): RTDomainCfg.Explicit =
              currentOwner.domainType match
                case DomainType.RT(cfg: RTDomainCfg.Explicit) => cfg
                case _ if currentOwner.isTop =>
                  RTDomainCfg.Explicit("main", co.defaultClkCfg, co.defaultRstCfg)
                case _ => getExplicitCfg(currentOwner.getOwnerDomain)
            val explicitCtg = getExplicitCfg(owner)
            val updatedOwner = owner match
              case design: DFDesignBlock => design.copy(domainType = DomainType.RT(explicitCtg))
              case domain: DomainBlock   => domain.copy(domainType = DomainType.RT(explicitCtg))
            Some(owner -> Patch.Replace(updatedOwner, Patch.Replace.Config.FullReplacement))
          case _ => None
      case _ => None
    }
    designDB.patch(patchList)
  end transform
end ExplicitClkRstCfg

extension [T: HasDB](t: T)
  def explicitClkRstCfg(using CompilerOptions): DB =
    StageRunner.run(ExplicitClkRstCfg)(t.db)
