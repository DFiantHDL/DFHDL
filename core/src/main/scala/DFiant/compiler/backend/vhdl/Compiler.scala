package DFiant
package compiler
package backend
package vhdl

import compiler.sync._
import DFiant.sim._
import scala.collection.mutable
import printer.formatter._

final class Compiler[D <: DFDesign, S <: shapeless.HList](c : IRCompilation[D, S]) {
  private val designDB =
    c.flattenNames
     .explicitPrev
     .explicitConversions
     .uniqueDesigns
     .uniqueNames(reservedKeywords, caseSensitive = false)
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
      case (cb : ConditionalBlock.ElseIfBlock, (closing, statements)) =>
        (If.ElsIf(Value.ref(cb.condRef.get), getProcessStatements(cb), closing), statements)
      case (cb : ConditionalBlock.IfBlock, (closing, statements)) =>
        ("", If(Value.ref(cb.condRef.get), getProcessStatements(cb), closing) :: statements)
      case (cb : ConditionalBlock.Case_Block[_], (_, statements)) =>
        (Case.When(Case.Choice.Others(), getProcessStatements(cb)), statements)
      case (cb : ConditionalBlock.CasePatternBlock[_], (whens, statements)) =>
        val when = Case.When(Case.Choice.Pattern(cb.pattern), getProcessStatements(cb))
        (if (whens.isEmpty) when else s"$when\n$whens", statements)
      case (mh : ConditionalBlock.MatchHeader, (whens, statements)) =>
        //TODO: handle matchval func @ vhdl93
        ("", Case(Value.ref(mh.matchValRef.get), whens) :: statements)
      case (Net.Internal(netStr), (closing, statements)) => (closing, netStr :: statements)
      case (Sim.Assert(assertStr), (closing, statements)) => (closing, assertStr :: statements)
      case (Sim.Finish(finishStr), (closing, statements)) => (closing, finishStr :: statements)
      case (_, nochange) => nochange
    }
    statements
  }
  def vhdlCompile[R <: Revision](implicit revision : R) = {
    implicit val printer : Printer = new Printer {
      val getSet : MemberGetSet = __getset
      val config : Printer.Config = new Printer.Config(revision)
    }
    val designTypes = mutable.Set.empty[String]
    val files = designDB.designMemberList.flatMap {
      case (design : DFDesign.Block.Internal, _) if design.inlinedRep.nonEmpty => None
      case (design : DFDesign.Block, members) if !designTypes.contains(design.designType) =>
        designTypes += design.designType
        val (ports, signals, variables) = members.foldRight((List.empty[String],List.empty[String],List.empty[String])){
          case (p @ DFAny.Port.In(), (ports, signals, variables)) =>
            (Port(p.name, Port.Dir.In(), Type(p), Init(p)) :: ports, signals, variables)
          case (p @ DFAny.Port.Out(), (ports, signals, variables)) =>
            (Port(p.name, Port.Dir.Out(), Type(p), Init(p)) :: ports, signals, variables)
          case (s : DFAny, (ports, signals, variables))
            if !s.isAnonymous && (
              designDB.getConnectionTo(s).isDefined ||
              s.tags.customTags.values.exists{
                case _ : Sync.Tag => true
                case _ => false
              } ||
              designDB.getAssignmentsFrom(s).exists(x => x.isTaggedWith(Sync.Tag.Reg))) =>
            (ports, Signal(s.name, Type(s), Init(s)) :: signals, variables)
          case (v : DFAny, (ports, signals, variables)) if !v.isAnonymous =>
            (ports, signals, Variable(v.name, Type(v), Init(v)) :: variables)
          case (_, psv) => psv
        }
        val entityName = design.designType
        val entity = Entity(entityName, ports)
        val componentInstances = members.collect {
          case x : DFDesign.Block.Internal if x.inlinedRep.isEmpty =>
            val connections = designDB.blockMemberTable(x).collect {
              case Net.External(netStr) => netStr
            }
            ComponentInstance(x.name, x.designType, connections)
        }
        val asyncStatements = getProcessStatements(design, !isSyncMember(_))
        val asyncSensitivityList : String = revision match {
          case Revision.V93 =>
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
            Process.Sensitivity.List(signalsOrPorts.map(e => e.name))
          case Revision.V2008 => Process.Sensitivity.All()
        }
        val asyncProcess = Process("async_proc", asyncSensitivityList, variables, asyncStatements)
        val syncStatements = getProcessStatements(design, isSyncMember)
        val syncSensitivityList = Process.Sensitivity.List(members.collect {
          case cb @ Sync.IfBlock(clkOrReset,_) if cb.getOwner == design => clkOrReset.name
          case Sync.ElseIfBlock(clk,_) => clk.name
        })
        val emits = members.collect {
          case Emitter(emitStr) => emitStr
        }.mkString("\n")
        val syncProcess = Process("sync_proc", syncSensitivityList, List(), syncStatements)
        val statements = componentInstances ++ List(asyncProcess, syncProcess, emits)
        val enumTypeDcls = designDB.getLocalEnumTypes(design).map(e => EnumTypeDcl(e)).toList
        val declarations = enumTypeDcls ++ signals
        val architecture = Architecture(s"${entityName}_arch", entityName, declarations, statements)
        val file = File(s"${designDB.top.designType}_pkg", entity, architecture)
        Some(BackendStage.File(s"${design.designType}.vhdl", s"$file"))
      case _ => None
    }
    val packageFile = BackendStage.File(s"${PackageFile.Name()}.vhdl", PackageFile())
    BackendStage.Compilation[D, Backend[R]](designDB, packageFile :: files)
  }
}