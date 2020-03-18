package ZFiant
package compiler
package backend
package vhdl

import compiler.sync._

import scala.collection.mutable

final class VHDLBackend[D <: DFDesign, S <: shapeless.HList](c : Compilable[D, S]) {
  private val designDB =
    c.explicitPrev
     .explicitConversions
     .uniqueDesigns
     .uniqueNames(reservedKeywords, caseSensitive = false)
     .clockedPrev
     .viaPortConnection
     .db

  import designDB.__getset
  private val isSyncMember : DFMember => Boolean = {
    case Sync.IfBlock() => true
    case cb: ConditionalBlock.ElseIfBlock => cb.condRef.get.getOwner match {
      case DFDesign.Block.Internal(_, _, _, Some(rep)) => rep match {
        case Rising.Rep(bitRef) => bitRef.get.tags.customTags.contains(Sync.Tag.Clk)
        case _ => false
      }
    }
    case net: DFNet => net.toRef.get.getOwner match {
      case DFDesign.Block.Internal(_, _, _, Some(rep)) => rep match {
        case Rising.Rep(bitRef) => bitRef.get.tags.customTags.contains(Sync.Tag.Clk)
        case _ => false
      }
      case _ => false
    }
    case _ => false
  }
  private def getProcessStatements(block : DFBlock, filterFunc : DFMember => Boolean = _ => true) : List[String] = {
    val (_, statements) = designDB.ownerMemberTable(block).filter(filterFunc).foldRight(("", List.empty[String])) {
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
        ("", Case(Value.ref(mh.matchValRef.get), whens) :: statements)
      case (_ : DFAny.Dcl, nochange) => nochange //declarations do not appear in processes
      case (a : DFAny, (closing, statements)) if !a.isAnonymous =>
        val netStr = Net.Assignment(a.name, Value(a))
        (closing, netStr :: statements)
      case (DFNet.Inlined(), nochange) => nochange //Inlined nets (connected to inlined blocks) do not appear in processes
      case (net : DFNet, (closing, statements)) if !net.hasLateConstruction =>
        val toValue = Value.ref(net.toRef.get)
        val fromValue = Value.ref(net.fromRef.get)
        val netStr = net match {
          case _ : DFNet.Assignment if !net.toRef.get.tags.customTags.contains(Sync.Tag.Reg) =>
            Net.Assignment(toValue, fromValue)
          case _ => Net.Connection(toValue, fromValue)
        }
        (closing, netStr :: statements)
      case (_, nochange) => nochange
    }
    statements
  }
  def compile = {
    val designTypes = mutable.Set.empty[String]
    val files = designDB.ownerMemberList.flatMap {
      case (design : DFDesign.Block.Internal, _) if design.inlinedRep.nonEmpty => None
      case (design : DFDesign.Block, members) if !designTypes.contains(design.designType) =>
        designTypes += design.designType
        val (ports, signals, variables) = members.foldRight((List.empty[String],List.empty[String],List.empty[String])){
          case (p @ DFAny.Port.In(), (ports, signals, variables)) =>
            (Port(p.name, Port.Dir.In(), Type(p), Init(p)) :: ports, signals, variables)
          case (p @ DFAny.Port.Out(), (ports, signals, variables)) =>
            (Port(p.name, Port.Dir.Out(), Type(p), Init(p)) :: ports, signals, variables)
          case (s : DFAny, (ports, signals, variables)) if designDB.getConnectionTo(s).isDefined || s.tags.customTags.contains(Sync.Tag.Reg) =>
            (ports, Signal(s.name, Type(s), Init(s)) :: signals, variables)
          case (v : DFAny, (ports, signals, variables)) if !v.isAnonymous =>
            (ports, signals, Variable(v.name, Type(v), Init(v)) :: variables)
          case (_, psv) => psv
        }
        val entityName = design.designType
        val entity = Entity(entityName, ports)
        val componentInstances = members.collect {
          case x : DFDesign.Block.Internal if x.inlinedRep.isEmpty =>
            val connections = designDB.ownerMemberTable(x).collect {
              case net : DFNet.Connection if net.hasLateConstruction =>
                val toVal = net.toRef.get
                val fromVal = net.fromRef.get
                if (toVal.isMemberOfDesign(x)) (toVal.name, fromVal.name) else (fromVal.name, toVal.name)
            }
            ComponentInstance(x.name, x.designType, connections)
        }
        val asyncStatements = getProcessStatements(design, !isSyncMember(_))
        val asyncProcess = Process("async_proc", Process.Sensitivity.All(), variables, asyncStatements)
        val syncStatements = getProcessStatements(design, isSyncMember)
        val syncProcess = Process("sync_proc", Process.Sensitivity.All(), List(), syncStatements)
        val statements = componentInstances ++ List(asyncProcess, syncProcess)
        val architecture = Architecture(s"${entityName}_arch", entityName, signals, statements)
        val file = File(s"${designDB.top.designType}_pack", entity, architecture)
        Some(Compilable.Cmd.GenFile(s"${design.designType}.vhdl", s"$file"))
      case _ => None
    }
    c.newStage[VHDLCompiler](designDB, c.cmdSeq ++ files)
  }
}

trait VHDLCompiler extends Compilable.Stage