package dfhdl.compiler.stages

import dfhdl.compiler.analysis.*
import dfhdl.compiler.ir.*
import dfhdl.compiler.patching.*
import dfhdl.options.CompilerOptions
import scala.annotation.tailrec
import scala.collection.mutable

/** This stage converts a derived clock-reset configuration of RT designs/domains to explicit
  * configurations.
  */
case object ExplicitClkRstCfg extends Stage:
  def dependencies: List[Stage] = List()
  def nullifies: Set[Stage] = Set()
  def transform(designDB: DB)(using MemberGetSet, CompilerOptions): DB =
    val domainMap = mutable.Map.empty[DFDomainOwner, RTDomainCfg.Explicit]
    extension (owner: DFDomainOwner)
      def usesClk: Boolean = designDB.domainOwnerMemberTable(owner).exists {
        case dcl: DFVal.Dcl           => dcl.modifier.reg
        case reg: DFVal.Alias.History => true
        case domain: DFDesignBlock =>
          domainMap.get(domain) match
            case Some(RTDomainCfg.Explicit(_, _: ClkCfg.Explicit, _)) => true
            case _                                                    => false
        case _ => false
      }
      def usesRst: Boolean = designDB.domainOwnerMemberTable(owner).exists {
        case dcl: DFVal.Dcl           => dcl.modifier.reg
        case reg: DFVal.Alias.History => true
        case domain: DFDesignBlock =>
          domainMap.get(domain) match
            case Some(RTDomainCfg.Explicit(_, _, _: RstCfg.Explicit)) => true
            case _                                                    => false
        case _ => false
      }
    end extension
    def getExplicitCfg(currentOwner: DFDomainOwner): RTDomainCfg.Explicit =
      currentOwner.domainType match
        case DomainType.RT(cfg: RTDomainCfg.Explicit) => cfg
        case DomainType.RT(RTDomainCfg.DerivedCfg) =>
          domainMap.get(currentOwner) match
            case Some(cfg) => cfg
            case None =>
              val cfg =
                if (currentOwner.isTop)
                  currentOwner.getTagOf[RTDomainCfg.Explicit].get
                else getExplicitCfg(currentOwner.getOwnerDomain)
              val updatedClkCfg: ClkCfg = if (currentOwner.usesClk) cfg.clkCfg else None
              val updatedRstCfg: RstCfg = if (currentOwner.usesRst) cfg.rstCfg else None
              val updateCfg = cfg.copy(clkCfg = updatedClkCfg, rstCfg = updatedRstCfg)
                .asInstanceOf[RTDomainCfg.Explicit]
              domainMap += currentOwner -> updateCfg
              updateCfg
        case DomainType.RT(RTDomainCfg.RelatedCfg(DFRef(relatedDomain))) =>
          getExplicitCfg(relatedDomain)
        case _ =>
          if (currentOwner.isTop)
            currentOwner.getTagOf[RTDomainCfg.Explicit].get
          else getExplicitCfg(currentOwner.getOwnerDomain)
      end match
    end getExplicitCfg
    val patchList: List[(DFMember, Patch)] = designDB.namedOwnerMemberList.flatMap {
      case (owner: (DFDomainOwner & DFBlock), members) =>
        owner.domainType match
          case domainType @ DomainType.RT(RTDomainCfg.DerivedCfg) =>
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
