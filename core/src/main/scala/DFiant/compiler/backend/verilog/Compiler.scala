package DFiant
package compiler
package backend
package verilog

import constraints.timing.sync.{ResetParams, ClockParams}
import DFiant.sim._
import scala.collection.mutable
import printer.formatter._
import RTL.Analysis

final class Compiler[D <: DFDesign](c : IRCompilation[D]) {
  private val designDB =
    c.flattenNames
      .explicitPrev
      .uniqueDesigns
      .fixAnonymous
      .namedSelection
      .uniqueNames(reservedKeywords + Sim.guardName, caseSensitive = true)
      .toRTLForm
      .viaPortConnection
      .orderMembers
      .db

  import designDB.__getset
  private val isSyncMember : DFMember => Boolean = {
    case RTL.IfBlock(_,_) | RTL.ElseIfBlock(_,_) => true
    case _ => false
  }

  private def getProcessStatements(block : DFBlock, filterFunc : DFMember => Boolean = _ => true)(
    implicit printer : Printer
  ) : List[String] = {
    import printer.config._
    val simGuardSetOption : Option[String] =
      if (printer.inSimulation) Some(Verilator.ifndef(s"${Sim.guardName} = $LIT 1;"))
      else None
    val (_, statements) = designDB.blockMemberTable(block).filter(filterFunc).foldRight(("", List.empty[String])) {
      case (cb : ConditionalBlock.ElseBlock, (_, statements)) =>
        (If.Else(getProcessStatements(cb)), statements)
      case (cb @ RTL.ElseIfBlock(_,_), (closing, statements)) =>
        (If.Else(getProcessStatements(cb) ++ simGuardSetOption), statements)
      case (cb : ConditionalBlock.ElseIfBlock, (closing, statements)) =>
        (If.ElsIf(Value.ref(cb.condRef.get), getProcessStatements(cb), closing), statements)
      case (cb @ RTL.IfBlock(RTL.IsClock(),_), (closing, statements)) =>
        ("", getProcessStatements(cb) ++ statements ++ simGuardSetOption)
      case (cb : ConditionalBlock.IfBlock, (closing, statements)) =>
        ("", If(Value.ref(cb.condRef.get), getProcessStatements(cb), closing) :: statements)
      case (cb : ConditionalBlock.Case_Block[_], (_, statements)) =>
        (Case.Item(Case.Choice.Default(), getProcessStatements(cb)), statements)
      case (cb : ConditionalBlock.CasePatternBlock[_], (items, statements)) =>
        val width = cb.matchHeaderRef.get.matchValRef.get.width
        val item = Case.Item(Case.Choice.Pattern(cb.pattern, width), getProcessStatements(cb))
        (if (items.isEmpty) item else s"$item\n$items", statements)
      case (mh : ConditionalBlock.MatchHeader, (items, statements)) =>
        ("", Case(Value.ref(mh.matchValRef.get), items) :: statements)
      case (Net.Internal(netStr), (closing, statements)) => (closing, netStr :: statements)
      case (Sim.Assert(assertStr), (closing, statements)) => (closing, assertStr :: statements)
      case (Sim.Finish(finishStr), (closing, statements)) => (closing, finishStr :: statements)
      case (_, nochange) => nochange
    }
    statements
  }
  def verilogCompile[R <: Revision](implicit revision : R) = {
    implicit val printer : Printer = new Printer {
      val getSet : MemberGetSet = __getset
      val config : Printer.Config = new Printer.Config(revision)
    }
    import printer.config.{KW, FN, LIT, OP}
    val designTypes = mutable.Set.empty[String]
    val files = designDB.designMemberList.flatMap {
      case (design : DFDesign.Block.Internal, _) if design.inlinedRep.nonEmpty => None
      case (design : DFDesign.Block, members) if !designTypes.contains(design.designType) =>
        designTypes += design.designType
        val (ports, wires, regs) = members.foldRight((List.empty[String],List.empty[String],List.empty[String])){
          case (p @ DFAny.Port.In(), (ports, wires, regs)) =>
            (Port(p.name, Port.Dir.In(), Type(p)) :: ports, wires, regs)
          case (p @ DFAny.Port.Out(), (ports, wires, regs)) =>
            (Port(p.name, Port.Dir.Out(), Type(p)) :: ports, wires, regs)
          case (RTL.IsClock() | RTL.IsReset(), pwr) if design.isTop && printer.inSimulation => pwr
          case (s : DFAny, (ports, wires, regs)) if !s.isAnonymous => designDB.getConnectionTo(s) match {
            case Some(v) if v.isPortOut => (ports, Wire(s.name, Type(s)) :: wires, regs)
            case _ => (ports, wires, Reg(s.name, Type(s), Init(s)) :: regs)
          }
          case (_, pwr) => pwr
        }
        val clkRstMembers = members.collect {
          case m @ RTL.IsClock() => m
          case m @ RTL.IsReset() => m
        }
        val enumInstances =
          if (printer.inSimulation) members.collect {
            case DFEnum(enumType) => enumType
          }.distinct.map(e => EnumInstance(e))
          else Nil
        val clkrstPorts = if (design.isTop && printer.inSimulation)
          Verilator.ifdef(clkRstMembers.map(cr => Port(cr.name, Port.Dir.In(), Type(cr))).mkString(",\n")) :: ports
        else ports
        object ClkSim {
          def unapply(clk : DFAny) : Option[String] = clk match {
            case RTL.IsClock() =>
              val reg = Reg(clk.name, Type(clk), Init(clk))
              val sim = s"$KW always #5 ${Value.ref(clk)} = $OP!${Value.ref(clk)};"
              Some(s"$reg\n$sim")
            case _ => None
          }
        }
        object RstSim {
          def unapply(rst : DFAny) : Option[String] = rst match {
            case RTL.IsReset() =>
              val reg = Reg(rst.name, Type(rst), Init(rst))
              val sim = s"$KW initial #10 ${Value.ref(rst)} = $LIT${ResetParams.get.inactiveInt};"
              Some(s"$reg\n$sim")
            case _ => None
          }
        }
        val simGuardRegOption : Option[String] =
          if (printer.inSimulation && clkRstMembers.nonEmpty) Some(Verilator.ifndef(Reg(Sim.guardName, "", s" = $LIT 0")))
          else None
        val simGuardClearOption : Option[String] =
          if (printer.inSimulation && clkRstMembers.nonEmpty) Some(Verilator.ifndef(s"${Sim.guardName} = $LIT 0;"))
          else None

        val clkrstRegs = if (design.isTop && printer.inSimulation)
          Verilator.ifndef(clkRstMembers.map{case ClkSim(s) => s case RstSim(s) => s}.mkString("\n")) :: regs ++ simGuardRegOption
        else regs ++ simGuardRegOption

        val moduleName = design.designType
        val moduleInstances = members.collect {
          case x : DFDesign.Block.Internal if x.inlinedRep.isEmpty =>
            val connections = designDB.blockMemberTable(x).collect {
              case Net.External(netStr) => netStr
            }
            ModuleInstance(x.name, x.designType, connections)
        }
        val asyncStatements = getProcessStatements(design, !isSyncMember(_)) ++ simGuardClearOption
        val asyncSensitivityList : String = revision match {
          case Revision.V95 => AlwaysBlock.Sensitivity.List(designDB.getSensitivityList(design))
          case Revision.V2005 => AlwaysBlock.Sensitivity.All()
        }
        val asyncBlock = AlwaysBlock(asyncSensitivityList, asyncStatements)
        val syncStatements = getProcessStatements(design, isSyncMember)
        val syncSensitivityList = AlwaysBlock.Sensitivity.List(members.collect {
          case cb @ RTL.IfBlock(clkOrRst, edge) if cb.getOwner == design =>
            if (edge) s"$KW posedge ${clkOrRst.name}"
            else s"$KW negedge ${clkOrRst.name}"
          case RTL.ElseIfBlock(clk, edge) =>
            if (edge) s"$KW posedge ${clk.name}"
            else s"$KW negedge ${clk.name}"
        })
        val emits = members.collect {
          case Emitter(emitStr) => emitStr
        }.mkString("\n")
        val syncProcess = AlwaysBlock(syncSensitivityList, syncStatements)
        val localEnumTypes = designDB.getLocalEnumTypes(design)
        val enumDefines = localEnumTypes.map(e => EnumTypeDcl.defines(e)).toList
        val enumModuleDcls =
          if (printer.inSimulation) localEnumTypes.map(e =>
            s"""/* verilator lint_off DECLFILENAME */
               |${EnumTypeDcl(e)}""".stripMargin
          ).toList
          else Nil
        val declarations =
          wires ++ clkrstRegs ++ enumDefines ++ enumInstances ++ moduleInstances :+ asyncBlock ++ syncProcess ++ emits
        val modules = enumModuleDcls :+ Module(moduleName, clkrstPorts, declarations)
        val file = File(GlobalDefsFile.Name(), "", modules.mkString(s"\n$EMPTY\n"))
        Some(BackendStage.File(s"${moduleName}.v", s"$file"))
      case _ => None
    }
    val globalDefsFile = BackendStage.File(s"${GlobalDefsFile.Name()}.v", GlobalDefsFile())
    BackendStage.Compilation[D, Backend[R]](c.dsn, designDB, globalDefsFile :: files)
  }
}