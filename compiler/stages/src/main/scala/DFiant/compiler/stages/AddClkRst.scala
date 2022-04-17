package DFiant.compiler.stages

import DFiant.compiler.analysis.*
import DFiant.compiler.ir.*
import DFiant.compiler.patching.*
import DFiant.internals.*
import scala.collection.mutable

/** This stage adds clock and reset signaling across the entire design
  */
case object AddClkRst extends Stage:
  def dependencies: List[Stage] = List(ToRT, ExplicitClkRstCfg)
  def nullifies: Set[Stage] = Set(ViaConnection)
  def transform(designDB: DB)(using MemberGetSet): DB =
    val patchList: List[(DFMember, Patch)] = designDB.namedOwnerMemberList.flatMap {
      case (owner: (DFDomainOwner & DFBlock), members) =>
        owner.domainType match
          case DomainType.RT(RTDomainCfg.Explicit(_, clkCfg, rstCfg)) =>
            val requiresClk = !(clkCfg equals None)
            val requiresRst = !(rstCfg equals None)
            val existingClk = members.collectFirst {
              case clk: DFVal.Dcl if clk.name == "clk" => clk
            }
            val existingRst = members.collectFirst {
              case clk: DFVal.Dcl if clk.name == "clk" => clk
            }
            val addClk = requiresClk && existingClk.isEmpty
            val addRst = requiresRst && existingRst.isEmpty
            if (addClk || addRst)
              val dsn = new MetaDesign():
                lazy val clk = DFBit <> IN setName "clk"
                if (addClk) clk // touch lazy clk to create
                lazy val rst = DFBit <> IN setName "rst"
                if (addRst) rst // touch lazy rst to create
              Some(owner -> Patch.Add(dsn, Patch.Add.Config.InsideFirst))
            else None
          case _ => None
      case _ => None
    }
    designDB.patch(patchList)
  end transform
end AddClkRst

extension [T: HasDB](t: T) def addClkRst: DB = StageRunner.run(AddClkRst)(t.db)
