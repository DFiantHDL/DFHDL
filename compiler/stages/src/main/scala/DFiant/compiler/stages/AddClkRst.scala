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
    val patchList: List[(DFMember, Patch)] = designDB.designMemberList.flatMap {
      // for all designs
      case (design, designMembers) =>
        val prevCfg = mutable.Set.empty[RTDomainCfg]
        (design :: designMembers).view
          // all the domain block owners, including the design block itself
          .collect { case o: (DFDomainOwner & DFBlock) => (o, designDB.namedOwnerMemberTable(o)) }
          .flatMap { case (owner, members) =>
            owner.domainType match
              // just register-transfer domains with a configuration that was not yet handled
              case DomainType.RT(cfg @ RTDomainCfg.Explicit(_, clkCfg, rstCfg))
                  if !prevCfg.contains(cfg) =>
                prevCfg += cfg // will not handle this again
                // clk and rst are required according to the configuration
                val requiresClk = clkCfg != None
                val requiresRst = rstCfg != None
                // collect existing clk and rst dataflow value members
                val existingClk = members.collectFirst {
                  case clk: DFVal.Dcl if clk.name == "clk" => clk
                }
                val existingRst = members.collectFirst {
                  case clk: DFVal.Dcl if clk.name == "clk" => clk
                }
                // need to add clk and rst flags
                val addClk = requiresClk && existingClk.isEmpty
                val addRst = requiresRst && existingRst.isEmpty
                if (addClk || addRst)
                  val dsn = new MetaDesign():
                    lazy val clk = DFBit <> IN setName "clk"
                    if (addClk) clk // touch lazy clk to create
                    lazy val rst = DFBit <> IN setName "rst"
                    if (addRst) rst // touch lazy rst to create
                  // the ports are added as first members
                  Some(owner -> Patch.Add(dsn, Patch.Add.Config.InsideFirst))
                else None
              case _ => None
          }
    }
    designDB.patch(patchList)
  end transform
end AddClkRst

extension [T: HasDB](t: T) def addClkRst: DB = StageRunner.run(AddClkRst)(t.db)
