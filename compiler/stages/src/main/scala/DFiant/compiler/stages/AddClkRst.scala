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
    val patchList = mutable.ListBuffer.empty[(DFMember, Patch)]
//    designDB.namedOwnerMemberList.foreach {
//      case (owner: (DFDomainOwner & DFBlock), members) =>
//        owner.domainType match
//          case domainType: DomainType.RT =>
//            domainType.clkCfg match
//              case ClkCfg.Explicit(clkName: String, _) =>
//
//            val RstCfg.Explicit(rstName: String, _, _) = domainType.rstCfg
//            members.collectFirst { case clk: DFVal.Dcl if clk.name == clkName => clk }.getOrElse {
//              val dsn = new MetaDesign() {}
//            }
//            ???
//          case _ => // do nothing
//      case _ => // do nothing
//    }
    designDB.patch(patchList.toList)
  end transform
end AddClkRst

extension [T: HasDB](t: T) def addClkRst: DB = StageRunner.run(AddClkRst)(t.db)
