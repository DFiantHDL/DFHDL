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
  private def getProcessStatements(block : DFBlock) : List[String] = {
    val (_, statements) = designDB.ownerMemberTable(block).foldRight(("", List.empty[String])) {
      case (cb : ConditionalBlock.ElseBlock, (_, statements)) =>
        (If.Else(getProcessStatements(cb)), statements)
      case (cb : ConditionalBlock.ElseIfBlock, (closing, statements)) =>
        (If.ElsIf(Value.ref(cb.condRef.get), getProcessStatements(cb), closing), statements)
      case (cb : ConditionalBlock.IfBlock, (closing, statements)) =>
        ("", If(Value.ref(cb.condRef.get), getProcessStatements(cb), closing) :: statements)
      case (cb : ConditionalBlock.Case_Block[_], (_, statements)) =>
        (Case.When(Case.Choice.Others(), getProcessStatements(cb)), statements)
      case (cb : ConditionalBlock.CasePatternBlock[_], (whens, statements)) =>
        (s"${Case.When(Case.Choice.Pattern(cb.pattern), getProcessStatements(cb))}\n$whens", statements)
      case (mh : ConditionalBlock.MatchHeader, (whens, statements)) =>
        ("", Case(Value.ref(mh.matchValRef.get), whens) :: statements)
      case (net : DFNet, ("", statements)) =>
        val toValue = Value.ref(net.toRef.get)
        val fromValue = Value.ref(net.fromRef.get)
        val netStr = net match {
          case a : DFNet.Assignment if !net.toRef.get.tags.customTags.contains(SyncTag.Reg) =>
            Net.Assignment(toValue, fromValue)
          case _ => Net.Connection(toValue, fromValue)
        }
        ("", netStr :: statements)
      case (_, keep) => keep
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
          case (s : DFAny, (ports, signals, variables)) if designDB.getConnectionTo(s).isDefined || s.tags.customTags.contains(SyncTag.Reg) =>
            (ports, Signal(s.name, Type(s), Init(s)) :: signals, variables)
          case (v @ DFAny.Var(), (ports, signals, variables)) =>
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
        val asyncStatements = getProcessStatements(design)
        val asyncProcess = Process("async_proc", Process.Sensitivity.All(), variables, asyncStatements)
        val statements = componentInstances :+ asyncProcess
        val architecture = Architecture(s"${entityName}_arch", entityName, signals, statements)
        val file = File(entity, architecture)
        Some(Compilable.Cmd.GenFile(s"${design.designType}.vhdl", s"$file"))
      case _ => None
    }
    c.newStage[VHDLCompiler](designDB, c.cmdSeq ++ files)
  }
}

trait VHDLCompiler extends Compilable.Stage