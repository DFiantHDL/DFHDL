package dfhdl.compiler.stages

import dfhdl.compiler.analysis.*
import dfhdl.compiler.ir.*
import dfhdl.compiler.patching.*
import dfhdl.internals.*
import dfhdl.options.CompilerOptions
import scala.collection.mutable

/** This connects clock and reset ports across the entire design
  */
case object ConnectClkRst extends Stage:
  def dependencies: List[Stage] = List(AddClkRst)
  def nullifies: Set[Stage] = Set(ViaConnection)
  private case class ClkRstOpt(clkOpt: Option[DFVal], rstOpt: Option[DFVal]) derives CanEqual
  def transform(designDB: DB)(using MemberGetSet, CompilerOptions): DB =
//    val domainClkRst = mutable.Map.empty[(DFDesignBlock, RTDomainCfg), ClkRstOpt]
//    designDB.namedOwnerMemberList.foreach {
//      case (owner: (DFDomainOwner & DFBlock), members) =>
//        owner.domainType match
//          case DomainType.RT(cfg @ RTDomainCfg.Explicit(_, clkCfg, rstCfg))
//              if clkCfg != None || rstCfg != None =>
//            val key = (owner.getThisOrOwnerDesign, cfg)
//            if (!domainClkRst.contains(key))
//              val existingClk = members.collectFirst {
//                case clk: DFVal.Dcl if clk.name == "clk" => clk
//              }
//              val existingRst = members.collectFirst {
//                case clk: DFVal.Dcl if clk.name == "clk" => clk
//              }
//              domainClkRst += key -> ClkRstOpt(existingClk, existingRst)
//          case _ => // do nothing
//      case _ => // do nothing
//    }
    val patchList: List[(DFMember, Patch)] = designDB.namedOwnerMemberList.flatMap {
      case (owner: (DFDomainOwner & DFBlock), members) =>
        owner.domainType match
          case DomainType.RT(RTDomainCfg.Explicit(_, clkCfg, rstCfg)) =>
            val requiresClk = !(clkCfg equals None)
            val requiresRst = !(rstCfg equals None)
            val existingClk = members.collectFirst {
              case clk: DFVal.Dcl if clk.getName == "clk" => clk
            }
            val existingRst = members.collectFirst {
              case clk: DFVal.Dcl if clk.getName == "clk" => clk
            }
            val addClk = requiresClk && existingClk.isEmpty
            val addRst = requiresRst && existingRst.isEmpty
            if (addClk || addRst)
              val dsn = new MetaDesign(owner, Patch.Add.Config.InsideFirst):
                lazy val clk = Bit <> IN setName "clk"
                if (addClk) clk // touch lazy clk to create
                lazy val rst = Bit <> IN setName "rst"
                if (addRst) rst // touch lazy rst to create
              Some(dsn.patch)
            else None
          case _ => None
      case _ => None
    }
    designDB.patch(patchList)
  end transform
end ConnectClkRst

extension [T: HasDB](t: T)
  def connectClkRst(using CompilerOptions): DB =
    StageRunner.run(ConnectClkRst)(t.db)
