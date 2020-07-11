package DFiant
package compiler
package backend
package verilog

import compiler.sync._
import DFiant.sim._
import scala.collection.mutable
import printer.formatter._

final class VerilogBackendOps[D <: DFDesign, S <: shapeless.HList](c : IRCompilation[D, S]) {
  private val designDB =
    c.flattenNames
      .explicitPrev
      .uniqueDesigns
      .fixAnonymous
      .namedSelection
      .uniqueNames(reservedKeywords, caseSensitive = true)
      .clockedPrev
      .viaPortConnection
      .db

  import designDB.__getset
  private val isSyncMember : DFMember => Boolean = {
    case Sync.IfBlock(_,_) | Sync.ElseIfBlock(_,_) => true
    case _ => false
  }
  private def getProcessStatements(block : DFBlock, filterFunc : DFMember => Boolean = _ => true)(
    implicit printer : Printer
  ) : List[String] = {
    val (_, statements) = designDB.blockMemberTable(block).filter(filterFunc).foldRight(("", List.empty[String])) {
      case (cb : ConditionalBlock.ElseBlock, (_, statements)) =>
        (If.Else(getProcessStatements(cb)), statements)
      case (cb @ Sync.ElseIfBlock(_,_), (closing, statements)) =>
        (If.Else(getProcessStatements(cb)), statements)
      case (cb : ConditionalBlock.ElseIfBlock, (closing, statements)) =>
        (If.ElsIf(Value.ref(cb.condRef.get), getProcessStatements(cb), closing), statements)
      case (cb @ Sync.IfBlock(Sync.IsClock(),_), (closing, statements)) =>
        ("", getProcessStatements(cb) ++ statements)
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
    val externalSIMPortsDefName = "VERILATOR"
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
          case (Sync.IsClock() | Sync.IsReset(), pwr) if design.isTop && printer.inSimulation => pwr
          case (s : DFAny, (ports, wires, regs)) if !s.isAnonymous => designDB.getConnectionTo(s) match {
            case Some(v) if v.isPortOut => (ports, Wire(s.name, Type(s)) :: wires, regs)
            case _ => (ports, wires, Reg(s.name, Type(s), Init(s)) :: regs)
          }
          case (_, pwr) => pwr
        }
        val clkRstMembers = members.collect {
          case m @ Sync.IsClock() => m
          case m @ Sync.IsReset() => m
        }
        val clkrstPorts = if (design.isTop && printer.inSimulation)
          s"""$FN`ifdef $externalSIMPortsDefName
             |${clkRstMembers.map(cr => Port(cr.name, Port.Dir.In(), Type(cr))).mkString(",\n").delim()}
             |$FN`endif
             |""".stripMargin :: ports
        else ports
        object ClkSim {
          def unapply(clk : DFAny) : Option[String] = clk match {
            case Sync.IsClock() =>
              val reg = Reg(clk.name, Type(clk), Init(clk))
              val sim = s"$KW always #5 ${Value.ref(clk)} = $OP!${Value.ref(clk)};"
              Some(s"$reg\n$sim")
            case _ => None
          }
        }
        object RstSim {
          def unapply(rst : DFAny) : Option[String] = rst match {
            case Sync.IsReset() =>
              val reg = Reg(rst.name, Type(rst), Init(rst))
              val inactiveVerilog = rst.tags.init.head.head match {
                case DFBool.Token(_,false,_) => "1"
                case DFBool.Token(_,true,_) => "0"
              }
              val sim = s"$KW initial #10 ${Value.ref(rst)} = $LIT$inactiveVerilog;"
              Some(s"$reg\n$sim")
            case _ => None
          }
        }
        val clkrstRegs = if (design.isTop && printer.inSimulation)
          s"""$FN`ifndef $externalSIMPortsDefName
             |${clkRstMembers.map{case ClkSim(s) => s case RstSim(s) => s}.mkString("\n").delim()}
             |$FN`endif""".stripMargin :: regs
        else regs

        val moduleName = design.designType
        val moduleInstances = members.collect {
          case x : DFDesign.Block.Internal if x.inlinedRep.isEmpty =>
            val connections = designDB.blockMemberTable(x).collect {
              case Net.External(netStr) => netStr
            }
            ModuleInstance(x.name, x.designType, connections)
        }
        val asyncStatements = getProcessStatements(design, !isSyncMember(_))
        val asyncSensitivityList : String = revision match {
          case Revision.V95 =>
            val producers = members.flatMap {
              case a @ DFNet.Assignment.Unref(_,fromVal,_,_) if !a.hasLateConstruction => Some(fromVal)
              case c @ DFNet.Connection.Unref(_,fromVal,_,_) if !c.hasLateConstruction => Some(fromVal)
              case DFAny.Func2.Unref(_,left,_,right,_,_) => List(left, right)
              case a : DFAny.Alias[_,_,_] => Some(a.relValRef.get)
              case DFSimMember.Assert.Unref(condOption,msg,_,_,_) => msg.seq ++ condOption
              case ifBlock : ConditionalBlock.IfBlock => Some(ifBlock.condRef.get)
              case elseIfBlock : ConditionalBlock.ElseIfBlock => Some(elseIfBlock.condRef.get)
              case mh : ConditionalBlock.MatchHeader => Some(mh.matchValRef.get)
              case _ => Nil
            }
            val signalsOrPorts = producers.distinct.collect {
              case p @ DFAny.Port.In() => p
              case v @ DFAny.NewVar() if v.isTaggedWith(Sync.Tag.Reg) => v
              case v @ DFAny.NewVar() if designDB.getAssignmentsTo(v).isEmpty => v
            }
            AlwaysBlock.Sensitivity.List(signalsOrPorts.map(e => e.name))
          case Revision.V2005 => AlwaysBlock.Sensitivity.All()
        }
        val asyncBlock = AlwaysBlock(asyncSensitivityList, asyncStatements)
        val syncStatements = getProcessStatements(design, isSyncMember)
        val syncSensitivityList = AlwaysBlock.Sensitivity.List(members.collect {
          case cb @ Sync.IfBlock(clkOrRst, edge) if cb.getOwner == design =>
            if (edge) s"$KW posedge ${clkOrRst.name}"
            else s"$KW negedge ${clkOrRst.name}"
          case Sync.ElseIfBlock(clk, edge) =>
            if (edge) s"$KW posedge ${clk.name}"
            else s"$KW negedge ${clk.name}"
        })
        val emits = members.collect {
          case Emitter(emitStr) => emitStr
        }.mkString("\n")
        val syncProcess = AlwaysBlock(syncSensitivityList, syncStatements)
        val enumTypeDcls = designDB.getLocalEnumTypes(design).map(e => EnumTypeDcl(e)).toList
        val declarations = enumTypeDcls ++ wires ++ clkrstRegs ++ moduleInstances :+ asyncBlock ++ syncProcess ++ emits
        val module = Module(moduleName, clkrstPorts, declarations)
        val file = File(GlobalDefsFile.Name(), "", module)
        Some(Backend.File(s"${moduleName}.v", s"$file"))
      case _ => None
    }
    val globalDefsFile = Backend.File(s"${GlobalDefsFile.Name()}.v", GlobalDefsFile())
    Backend.Compilation[D, VerilogBackend[R]](designDB, globalDefsFile :: files)
  }
}


sealed trait Revision extends Product with Serializable
object Revision {
  implicit case object V95 extends Revision
  type V95 = V95.type
  implicit case object V2005 extends Revision
  type V2005 = V2005.type
}

trait VerilogBackend[R <: Revision] extends Backend.Stage {
  def codeString : String = "verilog"
  val revision : R
}
object VerilogBackend extends VerilogBackend[Revision] {
  lazy val revision : Revision = ???
}