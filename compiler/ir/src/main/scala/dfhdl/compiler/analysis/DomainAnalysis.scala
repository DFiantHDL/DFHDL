package dfhdl.compiler.analysis
import dfhdl.compiler.ir.*
import dfhdl.internals.*
import scala.collection.mutable

/** Used to get context of existing clock and reset sources per design and domain configuration
  */
final class DomainAnalysis(designDB: DB):
  import designDB.getSet
  protected type DesignDomainKey = (DFDesignBlock, RTDomainCfg)
  protected final case class ClkRstOpt(clkOpt: Option[DFVal.Dcl], rstOpt: Option[DFVal.Dcl])
      derives CanEqual:
    private[DomainAnalysis] def addClk(clk: DFVal.Dcl): ClkRstOpt =
      if (clkOpt.isEmpty) copy(clkOpt = Some(clk)) else this
    private[DomainAnalysis] def addRst(rst: DFVal.Dcl): ClkRstOpt =
      if (rstOpt.isEmpty) copy(rstOpt = Some(rst)) else this

  extension (collectedDesignDomains: mutable.Map[DesignDomainKey, ClkRstOpt])
    private def isNotComplete(key: DesignDomainKey): Boolean =
      key._2 match
        case RTDomainCfg.Explicit(_, clkCfg, rstCfg) if clkCfg != None || rstCfg != None =>
          collectedDesignDomains.get(key) match
            case Some(clkRstOpt) =>
              (clkCfg != None && clkRstOpt.clkOpt.isEmpty) || (rstCfg != None && clkRstOpt.rstOpt.isEmpty)
            case None => true
        case _ =>
          setEmpty(key) // TODO: probably ugly to do this here
          false
    private def addClk(key: DesignDomainKey, clk: DFVal.Dcl): Unit =
      collectedDesignDomains += key -> collectedDesignDomains
        .get(key)
        .map(_.addClk(clk))
        .getOrElse(ClkRstOpt(Some(clk), None))
    private def addRst(key: DesignDomainKey, rst: DFVal.Dcl): Unit =
      collectedDesignDomains += key -> collectedDesignDomains
        .get(key)
        .map(_.addRst(rst))
        .getOrElse(ClkRstOpt(None, Some(rst)))
    private def setEmpty(key: DesignDomainKey): Unit =
      collectedDesignDomains += key -> ClkRstOpt(None, None)
  end extension

  val designDomains: Map[DesignDomainKey, ClkRstOpt] =
    val collectedDesignDomains = mutable.Map.empty[DesignDomainKey, ClkRstOpt]
    designDB.designMemberList.foreach { case (design, designMembers) => // for all designs
      (design :: designMembers).view
        // all the domain block owners, including the design block itself
        .collect { case o: (DFDomainOwner & DFBlock) => (o, designDB.namedOwnerMemberTable(o)) }
        .foreach { case (owner, members) =>
          owner.domainType match
            // just register-transfer domains with new configuration
            case DomainType.RT(cfg: RTDomainCfg.Explicit)
                if collectedDesignDomains.isNotComplete((design, cfg)) =>
              // collect existing clk and rst DFHDL value members
              members.collectFirst {
                case clk: DFVal.Dcl if clk.getName == "clk" =>
                  // if clk is an output then this is an output domain.
                  // if the design is not a top-level, then the design owner is the receiver of this domain.
                  if (clk.modifier.dir == DFVal.Modifier.OUT && !design.isTop)
                    collectedDesignDomains.addClk((design.getOwnerDesign, cfg), clk)
                  collectedDesignDomains.addClk((design, cfg), clk)
              }
              members.collectFirst {
                case rst: DFVal.Dcl if rst.getName == "rst" =>
                  // if rst is an output then this is an output domain.
                  // if the design is not a top-level, then the design owner is the receiver of this domain.
                  if (rst.modifier.dir == DFVal.Modifier.OUT && !design.isTop)
                    collectedDesignDomains.addRst((design.getOwnerDesign, cfg), rst)
                  collectedDesignDomains.addRst((design, cfg), rst)
              }
            case _ => // do nothing
          end match
        }
    }
    collectedDesignDomains.toMap
  end designDomains
end DomainAnalysis
