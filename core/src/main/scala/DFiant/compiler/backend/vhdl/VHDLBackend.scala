package DFiant
package compiler
package backend
package vhdl

import compiler.sync._

import scala.collection.mutable
sealed trait VHDLRevision extends Product with Serializable
object VHDLRevision {
  case object VHDL1993 extends VHDLRevision
  implicit case object VHDL2008 extends VHDLRevision
}

final class VHDLBackend[D <: DFDesign, S <: shapeless.HList](c : Compilable[D, S]) {
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
    case Sync.IfBlock(_) | Sync.ElseIfBlock(_) => true
    case _ => false
  }
  private def getProcessStatements(block : DFBlock, filterFunc : DFMember => Boolean = _ => true)(implicit printer : Printer, revision : VHDLRevision) : List[String] = {
    import printer.config.formatter._
    val (_, statements) = designDB.blockMemberTable(block).filter(filterFunc).foldRight(("", List.empty[String])) {
      case (cb : ConditionalBlock.ElseBlock, (_, statements)) =>
        (If.Else(getProcessStatements(cb)), statements)
      case (cb : ConditionalBlock.ElseIfBlock, (closing, statements)) =>
        (If.ElsIf(Value.boolRef(cb.condRef.get), getProcessStatements(cb), closing), statements)
      case (cb : ConditionalBlock.IfBlock, (closing, statements)) =>
        ("", If(Value.boolRef(cb.condRef.get), getProcessStatements(cb), closing) :: statements)
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
  def compile(implicit revision : VHDLRevision) = {
    val designTypes = mutable.Set.empty[String]
    val files = designDB.blockMemberList.flatMap {
      case (design : DFDesign.Block.Internal, _) if design.inlinedRep.nonEmpty => None
      case (design : DFDesign.Block, members) if !designTypes.contains(design.designType) =>
        designTypes += design.designType
        val (ports, signals, variables) = members.foldRight((List.empty[String],List.empty[String],List.empty[String])){
          case (p @ DFAny.Port.In(), (ports, signals, variables)) =>
            (Port(p.name, Port.Dir.In(), Type(p), Init(p)) :: ports, signals, variables)
          case (p @ DFAny.Port.Out(), (ports, signals, variables)) =>
            (Port(p.name, Port.Dir.Out(), Type(p), Init(p)) :: ports, signals, variables)
          case (s : DFAny, (ports, signals, variables))
            if designDB.getConnectionTo(s).isDefined
              || s.tags.customTags.exists{case _ : Sync.Tag => true}
              || designDB.getAssignmentsFrom(s).exists(x => x.tags.customTags.contains(Sync.Tag.Reg)) =>
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
          case VHDLRevision.VHDL1993 =>
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
              case v @ DFAny.NewVar() if v.tags.customTags.contains(Sync.Tag.Reg) => v
              case v @ DFAny.NewVar() if designDB.getAssignmentsTo(v).isEmpty => v
            }
            Process.Sensitivity.List(signalsOrPorts.map(e => e.name))
          case VHDLRevision.VHDL2008 => Process.Sensitivity.All()
        }
        val asyncProcess = Process("async_proc", asyncSensitivityList, variables, asyncStatements)
        val syncStatements = getProcessStatements(design, isSyncMember)
        val syncSensitivityList = Process.Sensitivity.List(members.collect {
          case Sync.IfBlock(clkOrReset) => clkOrReset.name
          case Sync.ElseIfBlock(clk) => clk.name
        })
        val emits = members.collect {
          case Emitter(emitStr) => emitStr
        }.mkString("\n")
        val syncProcess = Process("sync_proc", syncSensitivityList, List(), syncStatements)
        val statements = componentInstances ++ List(asyncProcess, syncProcess, emits)
        val architecture = Architecture(s"${entityName}_arch", entityName, signals, statements)
        val file = File(s"${designDB.top.designType}_pkg", entity, architecture)
        Some(Compilable.Cmd.GenFile(s"${design.designType}.vhdl", s"$file"))
      case _ => None
    }
    val packageFile = Compilable.Cmd.GenFile(s"${PackageFile.Name()}.vhdl", PackageFile())
    c.newStage[VHDLCompiler](designDB, c.cmdSeq ++ (packageFile :: files))
  }
}


trait VHDLCompiler extends Compilable.Stage