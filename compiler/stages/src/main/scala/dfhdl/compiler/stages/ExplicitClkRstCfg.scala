package dfhdl.compiler.stages

import dfhdl.compiler.analysis.*
import dfhdl.compiler.ir.*
import dfhdl.compiler.patching.*
import dfhdl.options.CompilerOptions
import dfhdl.internals.invert
import scala.annotation.tailrec
import scala.collection.mutable
import dfhdl.core.{refTW, DFC}

/** This stage converts a derived clock-reset configuration of RT designs/domains to
  * explicit/related configurations. The rules are (when a derived configuration is discovered):
  *   1. For a design, we first check its contents for Clk/Rst "usage", and if they are in use, the
  *      explicit configuration is taken from its owner, if such exists. If its owner is an ED
  *      domain or the design is top-level, the elaboration options' default configuration is used.
  *      If no Clk/Rst "usage" is detected, then the explicit configuration is set to None. Clk/Rst
  *      "usage" is indicated by any of the following:
  *      - a register declaration (for rst, a non-bubble init must be used to trigger rst usage)
  *      - a register alias (for rst, a non-bubble init must be used to trigger rst usage)
  *      - an internal design that has explicit Clk/Rst configuration
  *      - an explicit Clk/Rst declaration by the user
  *      - a top design default configuration's inclusion policy is set to `AlwaysAtTop`
  *   1. For a domain with non-Clk/Rst input port members, the explicit configuration is taken from
  *      its input source. The input source is determined by searching for a source that is
  *      registered and get its register's owner domain, and consequently its configuration. An
  *      earlier elaboration check should prevent any ambiguity from the source. One such possible
  *      ambiguity is having two domains that provide inputs and outputs to each other and both are
  *      declared with derived configuration. In this case, we get a circular derived configuration
  *      dependency, which should yield an error during elaboration.
  *   1. For a domain with no input port members, the domain is considered to be a related domain of
  *      the domain's owner and the configuration is set accordingly.
  *   1. When deriving a no-Rst configuration from a with-Rst configuration `cfg`, the derived
  *      config is set as `cfg.norst`, with the name mangling `${cfg.name}.norst`. This name
  *      mangling is special-cased in various stages and compiler logic and used to indicated that
  *      both domain configurations are derived from one another.
  */
case object ExplicitClkRstCfg extends Stage:
  def dependencies: List[Stage] = List(UniqueDesigns)
  def nullifies: Set[Stage] = Set(DropUnreferencedAnons)
  def transform(designDB: DB)(using MemberGetSet, CompilerOptions): DB =
    val relatedCfgRefs = mutable.Map.empty[DFRefAny, DFMember]
    given dfc: DFC = DFC.emptyNoEO
    val patchList: List[(DFMember, Patch)] = designDB.namedOwnerMemberList.flatMap {
      case (owner: (DFDomainOwner & DFBlock), members) =>
        owner.domainType match
          case domainType @ DomainType.RT(RTDomainCfg.Derived) =>
            val explicitCfg = designDB.explicitRTDomainCfgMap(owner)
            owner match
              case domain: DomainBlock =>
                val domainOwner = domain.getOwnerDomain
                val updatedCfg =
                  if (domain.isDependentOn(domainOwner))
                    val ref =
                      domainOwner.asInstanceOf[DomainBlock | DFDesignBlock].refTW[DomainBlock]
                    relatedCfgRefs += ref -> domainOwner
                    RTDomainCfg.Related(ref)
                  else explicitCfg
                val updatedDomain = domain.copy(domainType = DomainType.RT(updatedCfg))
                Some(domain -> Patch.Replace(updatedDomain, Patch.Replace.Config.FullReplacement))
              case design: DFDesignBlock =>
                val updatedDesign = design.copy(domainType = DomainType.RT(explicitCfg))
                Some(design -> Patch.Replace(updatedDesign, Patch.Replace.Config.FullReplacement))
            end match
          case _ => None
      case _ => None
    }
    designDB.copy(refTable = designDB.refTable ++ relatedCfgRefs).patch(patchList)
  end transform
end ExplicitClkRstCfg

extension [T: HasDB](t: T)
  def explicitClkRstCfg(using CompilerOptions): DB =
    StageRunner.run(ExplicitClkRstCfg)(t.db)
