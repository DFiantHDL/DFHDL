package dfhdl.compiler.analysis
import dfhdl.compiler.ir.*
import dfhdl.internals.*
import scala.collection.mutable

/** Used to get context of existing clock and reset sources per design and domain configuration
  */
final class DomainAnalysis(designDB: DB):
  import designDB.getSet
  protected final case class ClkRstOpt(clkOpt: Option[DFVal.Dcl], rstOpt: Option[DFVal.Dcl])
      derives CanEqual:
    private[DomainAnalysis] def addClk(clk: DFVal.Dcl): ClkRstOpt =
      if (clkOpt.isEmpty) copy(clkOpt = Some(clk)) else this
    private[DomainAnalysis] def addRst(rst: DFVal.Dcl): ClkRstOpt =
      if (rstOpt.isEmpty) copy(rstOpt = Some(rst)) else this

  extension (collectedDesignDomains: mutable.Map[DFDomainOwner, ClkRstOpt])
    private def isNotComplete(key: DFDomainOwner): Boolean =
      key.domainType match
        case DomainType.RT(RTDomainCfg.Explicit(_, clkCfg, rstCfg))
            if clkCfg != None || rstCfg != None =>
          collectedDesignDomains.get(key) match
            case Some(clkRstOpt) =>
              (clkCfg != None && clkRstOpt.clkOpt.isEmpty) || (rstCfg != None && clkRstOpt.rstOpt.isEmpty)
            case None => true
        case _ =>
          setEmpty(key) // TODO: probably ugly to do this here
          false
    private def addClk(key: DFDomainOwner, clk: DFVal.Dcl): Unit =
      collectedDesignDomains += key -> collectedDesignDomains
        .get(key)
        .map(_.addClk(clk))
        .getOrElse(ClkRstOpt(Some(clk), None))
    private def addRst(key: DFDomainOwner, rst: DFVal.Dcl): Unit =
      collectedDesignDomains += key -> collectedDesignDomains
        .get(key)
        .map(_.addRst(rst))
        .getOrElse(ClkRstOpt(None, Some(rst)))
    private def setEmpty(key: DFDomainOwner): Unit =
      collectedDesignDomains += key -> ClkRstOpt(None, None)
  end extension

  val designDomains: Map[DFDomainOwner, ClkRstOpt] =
    val collectedDesignDomains = mutable.Map.empty[DFDomainOwner, ClkRstOpt]
    def collectDomainClkRst(owner: DFDomainOwner, members: List[DFMember]): Unit =
      val design = owner.getThisOrOwnerDesign
      owner.domainType match
        // just register-transfer domains with new configuration
        case DomainType.RT(cfg: RTDomainCfg.Explicit)
            if collectedDesignDomains.isNotComplete(owner) =>
          // collect existing clk and rst DFHDL value members
          members.collectFirst {
            case clk: DFVal.Dcl if clk.isClkDcl =>
              // if clk is an output then this is an output domain.
              // if the design is not a top-level, then the design owner is the receiver of this domain.
              if (clk.modifier.dir == DFVal.Modifier.OUT && !design.isTop)
                collectedDesignDomains.addClk(design.getOwnerDesign, clk)
              collectedDesignDomains.addClk(owner, clk)
          }
          members.collectFirst {
            case rst: DFVal.Dcl if rst.isRstDcl =>
              // if rst is an output then this is an output domain.
              // if the design is not a top-level, then the design owner is the receiver of this domain.
              if (rst.modifier.dir == DFVal.Modifier.OUT && !design.isTop)
                collectedDesignDomains.addRst(design.getOwnerDesign, rst)
              collectedDesignDomains.addRst(owner, rst)
          }
        case DomainType.RT(RTDomainCfg.Related(DFRef(relatedDomain))) =>
          // we normally analyze domains bottom up, but a related domain that depends on its
          // owner design first requires us to analyze the design
          if (owner.isInsideOwner(relatedDomain))
            collectDomainClkRst(relatedDomain, designDB.domainOwnerMemberTable(relatedDomain))
          collectedDesignDomains += owner -> collectedDesignDomains(relatedDomain)
        case _ => // do nothing
      end match
    end collectDomainClkRst

    designDB.domainOwnerMemberList.foreach { collectDomainClkRst }
    collectedDesignDomains.toMap
  end designDomains
end DomainAnalysis
