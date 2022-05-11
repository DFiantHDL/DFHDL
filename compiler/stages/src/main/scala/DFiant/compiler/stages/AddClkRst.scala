package DFiant.compiler.stages

import DFiant.compiler.analysis.*
import DFiant.compiler.ir.*
import DFiant.compiler.patching.*
import DFiant.internals.*
import scala.collection.mutable

/** This stage adds clock and reset ports across the entire design. For each design, clock and reset
  * ports are added once per unique domain configuration.
  */
case object AddClkRst extends Stage:
  def dependencies: List[Stage] = List(ToRT, ExplicitClkRstCfg)
  def nullifies: Set[Stage] = Set(ViaConnection)
  def transform(designDB: DB)(using MemberGetSet): DB =
    // saves domains that are outputting clk and rst
    val designDomainOut = mutable.Set.empty[(DFDesignBlock, RTDomainCfg)]
    val patchList: List[(DFMember, Patch)] = designDB.designMemberList.flatMap {
      // for all designs
      case (design, designMembers) =>
        // new configuration unapply object
        object NewCfg:
          // memoize previously handled domain configurations
          val prevCfg = mutable.Set.empty[RTDomainCfg]
          // a configuration that was not yet handled either by the design itself,
          // by a different domain, or by an internal design that has a domain output
          def unapply(cfg: RTDomainCfg.Explicit): Option[(ClkCfg, RstCfg)] =
            val RTDomainCfg.Explicit(_, clkCfg, rstCfg) = cfg
            if (!prevCfg.contains(cfg) && !designDomainOut.contains((design, cfg)))
              prevCfg += cfg // will not handle this again
              Some(clkCfg, rstCfg)
            else None
        end NewCfg
        (design :: designMembers).view
          // all the domain block owners, including the design block itself
          .collect { case o: (DFDomainOwner & DFBlock) => (o, designDB.namedOwnerMemberTable(o)) }
          .flatMap { case (owner, members) =>
            val ownerDomainPatchOption = owner.domainType match
              // just register-transfer domains with new configuration
              case DomainType.RT(cfg @ NewCfg(clkCfg, rstCfg)) =>
                // clk and rst are required according to the configuration
                val requiresClk = clkCfg != None
                val requiresRst = rstCfg != None
                // collect existing clk and rst dataflow value members
                val existingClk = members.collectFirst {
                  case clk: DFVal.Dcl if clk.name == "clk" =>
                    // if clk is an output then this is an output domain.
                    // if the design is not a top-level, then the design owner is the receiver of this domain.
                    if (clk.modifier == DFVal.Modifier.OUT && !design.isTop)
                      designDomainOut += Tuple2(design.getOwnerDesign, cfg)
                    clk
                }
                val existingRst = members.collectFirst {
                  case rst: DFVal.Dcl if rst.name == "rst" => rst
                }
                // need to add clk and rst flags
                val addClk = requiresClk && existingClk.isEmpty
                val addRst = requiresRst && existingRst.isEmpty
                if (addClk || addRst)
                  val dsn = new MetaDesign():
                    lazy val clk = Bit <> IN setName "clk"
                    if (addClk) clk // touch lazy clk to create
                    lazy val rst = Bit <> IN setName "rst"
                    if (addRst) rst // touch lazy rst to create
                  // the ports are added as first members
                  Some(owner -> Patch.Add(dsn, Patch.Add.Config.InsideFirst))
                else None
              case _ => None
            // register aliases and declarations can have explicit domain dependency, so for those
            // configurations we need to create domains at the design level (if they don't exist)
            val regDomainPatch = members.collect { case RegDomain(cfg @ NewCfg(clkCfg, rstCfg)) =>
              val cfgName = cfg.getName + "Dmn"
              val dsn = new MetaDesign():
                val rtDomain = new RTDomain(cfg)(using dfc.setName(cfgName)):
                  lazy val clk = Bit <> IN setName s"clk"
                  if (clkCfg != None) clk // touch lazy clk to create
                  lazy val rst = Bit <> IN setName s"rst"
                  if (rstCfg != None) rst // touch lazy rst to create
                rtDomain.onCreateEnd // need to run manually since plugin is not enabled here
              // the ports are added as first members
              design -> Patch.Add(dsn, Patch.Add.Config.InsideFirst)
            }
            List(
              ownerDomainPatchOption,
              regDomainPatch
            ).flatten
          }
    }
    designDB.patch(patchList)
  end transform
end AddClkRst

extension [T: HasDB](t: T) def addClkRst: DB = StageRunner.run(AddClkRst)(t.db)
