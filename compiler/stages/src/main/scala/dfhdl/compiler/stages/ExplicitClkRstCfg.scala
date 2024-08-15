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
    // mapping designs and their uses of Clk/Rst
    // we use the design declaration name (after enforcing uniqueness) because designs
    // can be empty duplicates and the indications need to come from the full designs
    //                                       dclName  usesClk  usesRst
    val designUsesClkRst = mutable.Map.empty[String, (Boolean, Boolean)]
    //                                                            usesClk  usesRst
    val domainOwnerUsesClkRst = mutable.Map.empty[DFDomainOwner, (Boolean, Boolean)]
    val reversedDependents = designDB.dependentRTDomainOwners.invert
    extension (domainOwner: DFDomainOwner)
      // true if the domainOwner is dependent at any level of thatDomainOwner's configuration
      @tailrec def isDependentOn(thatDomainOwner: DFDomainOwner): Boolean =
        designDB.dependentRTDomainOwners.get(domainOwner) match
          case Some(dependency) =>
            if (dependency == thatDomainOwner) true
            else dependency.isDependentOn(thatDomainOwner)
          case None => false
      def usesClkRst: (Boolean, Boolean) = domainOwner match
        case design: DFDesignBlock =>
          designUsesClkRst.getOrElseUpdate(
            design.dclName,
            design.domainType match
              case DomainType.RT(RTDomainCfg.Explicit(_, clkCfg, rstCfg)) =>
                (clkCfg != None, rstCfg != None)
              case _ => (design.usesClk, design.usesRst)
          )
        case _ =>
          domainOwnerUsesClkRst.getOrElseUpdate(
            domainOwner,
            (domainOwner.usesClk, domainOwner.usesRst)
          )
      def usesClk: Boolean = designDB.domainOwnerMemberTable(domainOwner).exists {
        case dcl: DFVal.Dcl           => dcl.modifier.isReg || dcl.isClkDcl
        case reg: DFVal.Alias.History => true
        case internal: DFDesignBlock  => internal.usesClkRst._1
        case _                        => false
      } || reversedDependents.getOrElse(domainOwner, Set()).exists(_.usesClkRst._1)
      def usesRst: Boolean = designDB.domainOwnerMemberTable(domainOwner).exists {
        case dcl: DFVal.Dcl =>
          (dcl.modifier.isReg && dcl.hasNonBubbleInit) || dcl.isRstDcl
        case reg: DFVal.Alias.History => reg.hasNonBubbleInit
        case internal: DFDesignBlock  => internal.usesClkRst._2
        case _                        => false
      } || reversedDependents.getOrElse(domainOwner, Set()).exists(_.usesClkRst._2)
    end extension

    // filling domain to configuration map
    val domainMap = mutable.Map.empty[DFDomainOwner, RTDomainCfg.Explicit]
    extension (cfg: RTDomainCfg.Explicit)
      // derived design configuration can be relaxed to no-Clk/Rst according to its
      // internal usage, as determined by `usesClkRst`
      def relaxed(atDomain: DFDomainOwner): RTDomainCfg.Explicit =
        val (usesClk, usesRst) = atDomain.usesClkRst
        val updatedClkCfg: ClkCfg = if (usesClk) cfg.clkCfg else None
        val updatedRstCfg: RstCfg = if (usesRst) cfg.rstCfg else None
        val updatedName =
          if (!usesClk) s"RTDomainCfg.Comb"
          else if (cfg.clkCfg != None && !usesRst)
            s"${cfg.name}.norst"
          else cfg.name
        RTDomainCfg.Explicit(updatedName, updatedClkCfg, updatedRstCfg)
          .asInstanceOf[RTDomainCfg.Explicit]
    end extension

    @tailrec def fillDomainMap(domains: List[DFDomainOwner], stack: List[DFDomainOwner]): Unit =
      domains match
        // already has configuration for this domain -> skip it
        case domain :: rest if domainMap.contains(domain) => fillDomainMap(rest, stack)
        // no configuration for this domain
        case domain :: rest =>
          // check if the domain is dependent
          designDB.dependentRTDomainOwners.get(domain) match
            // the domain is dependent -> its configuration is set by the dependency
            case Some(dependencyDomain) =>
              domainMap.get(dependencyDomain) match
                // found dependency configuration -> save it to the current domain as well
                case Some(dependencyConfig) =>
                  domainMap += domain -> dependencyConfig.relaxed(domain)
                  fillDomainMap(rest, stack)
                // missing dependency -> put this domain in the stack for now
                case None => fillDomainMap(rest, domain :: stack)
              end match
            // the domain is independent -> explicit configuration is set according to other factors
            case _ =>
              val explicitCfg = domain.domainType match
                case DomainType.RT(explicitCfg: RTDomainCfg.Explicit) => explicitCfg
                case _ => designDB.top.getTagOf[RTDomainCfg.Explicit].get
              domainMap += domain -> explicitCfg.relaxed(domain)
              fillDomainMap(rest, stack)
          end match
        // no more domains, but there are left in the stack
        case Nil if stack.nonEmpty => fillDomainMap(domains = stack, Nil)
        // we're done!
        case _ =>
      end match
    end fillDomainMap
    val derivedDomainOwners = designDB.domainOwnerMemberList.map(_._1)
    fillDomainMap(derivedDomainOwners, Nil)
    val relatedCfgRefs = mutable.Map.empty[DFRefAny, DFMember]
    given dfc: DFC = DFC.emptyNoEO
    val patchList: List[(DFMember, Patch)] = designDB.namedOwnerMemberList.flatMap {
      case (owner: (DFDomainOwner & DFBlock), members) =>
        owner.domainType match
          case domainType @ DomainType.RT(RTDomainCfg.Derived) =>
            val explicitCfg = domainMap(owner)
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
