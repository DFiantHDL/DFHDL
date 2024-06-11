package dfhdl.compiler.stages

import dfhdl.compiler.analysis.*
import dfhdl.compiler.ir.*
import dfhdl.compiler.patching.*
import dfhdl.internals.*
import dfhdl.options.CompilerOptions
import scala.collection.mutable
import dfhdl.core.DFOpaque as coreDFOpaque
import dfhdl.core.asFE

/** This stage adds clock and reset ports across the entire design. For each design, clock and reset
  * ports are added once per unique domain configuration.
  */
case object AddClkRst extends Stage:
  def dependencies: List[Stage] = List(ToRT, ExplicitClkRstCfg)
  def nullifies: Set[Stage] = Set(ViaConnection)
  def transform(designDB: DB)(using MemberGetSet, CompilerOptions): DB =
    // saves domains that are outputting clk and rst
    val designDomainOut = mutable.Set.empty[(DFDesignBlock, RTDomainCfg)]
    // saves clk type used for a domain
    val clkTypeMap = mutable.Map.empty[RTDomainCfg.Explicit, coreDFOpaque[coreDFOpaque.Clk]]
    // saves rst type used for a domain
    val rstTypeMap = mutable.Map.empty[RTDomainCfg.Explicit, coreDFOpaque[coreDFOpaque.Rst]]
    // go through all dcls and memoize clk/rst types
    designDB.members.foreach {
      case dcl: DFVal.Dcl =>
        dcl.dfType match
          case dfType @ DFOpaque(_, id: (DFOpaque.Clk | DFOpaque.Rst), _) =>
            dcl.getOwnerDomain.domainType match
              case DomainType.RT(cfg: RTDomainCfg.Explicit) =>
                id match
                  case _: DFOpaque.Clk =>
                    clkTypeMap += cfg -> dfType.asFE[coreDFOpaque[coreDFOpaque.Clk]]
                  case _: DFOpaque.Rst =>
                    rstTypeMap += cfg -> dfType.asFE[coreDFOpaque[coreDFOpaque.Rst]]
              case _ =>
          case _ =>
      case _ =>
    }
    val patchList: List[(DFMember, Patch)] = designDB.designMemberList.flatMap {
      // for all designs
      case (design, designMembers) =>
        // new configuration unapply object
        object NewCfg:
          // memoize previously handled domain configurations
          val prevCfg = mutable.Set.empty[RTDomainCfg.Explicit]
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
                // collect existing clk and rst DFHDL value members
                val existingClk = members.collectFirst {
                  case clk: DFVal.Dcl if clk.isClkDcl =>
                    // if clk is an output then this is an output domain.
                    // if the design is not a top-level, then the design owner is the receiver of this domain.
                    if (clk.modifier.dir == DFVal.Modifier.OUT && !design.isTop)
                      designDomainOut += Tuple2(design.getOwnerDesign, cfg)
                    clk
                }
                val existingRst = members.collectFirst {
                  case rst: DFVal.Dcl if rst.isRstDcl => rst
                }
                // need to add clk and rst flags
                val addClk = requiresClk && existingClk.isEmpty
                val addRst = requiresRst && existingRst.isEmpty
                // clk and rst magnet types are either fetched from the memoization
                // or created and memoized
                lazy val clkType =
                  class Unique:
                    case class Clk() extends coreDFOpaque.Clk:
                      override lazy val typeName: String = s"Clk_${cfg.name}"
                  val unique = new Unique
                  clkTypeMap.getOrElseUpdate(cfg, coreDFOpaque(unique.Clk()))
                lazy val rstType =
                  class Unique:
                    case class Rst() extends coreDFOpaque.Rst:
                      override lazy val typeName: String = s"Rst_${cfg.name}"
                  val unique = new Unique
                  rstTypeMap.getOrElseUpdate(cfg, coreDFOpaque(unique.Rst()))
                if (addClk || addRst)
                  val dsn = new MetaDesign(owner, Patch.Add.Config.InsideFirst):
                    lazy val clk = (clkType <> IN)(using dfc.setName("clk"))
                    if (addClk) clk // touch lazy clk to create
                    lazy val rst = (rstType <> IN)(using dfc.setName("rst"))
                    if (addRst) rst // touch lazy rst to create
                  // the ports are added as first members
                  Some(dsn.patch)
                else None
              case _ => None
            List(
              ownerDomainPatchOption
            ).flatten
          }
    }
    designDB.patch(patchList)
  end transform
end AddClkRst

extension [T: HasDB](t: T)
  def addClkRst(using CompilerOptions): DB = StageRunner.run(AddClkRst)(t.db)
