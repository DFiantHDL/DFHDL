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

  def compile = {
    val designTypes = mutable.Set.empty[String]
    val files = designDB.designMemberList.flatMap {
      case (design : DFDesign.Block.Internal, _) if design.inlinedRep.nonEmpty => None
      case (design, members) if !designTypes.contains(design.designType) =>
        designTypes += design.designType
        val (ports, signals, variables) = members.foldLeft((List.empty[String],List.empty[String],List.empty[String])){
          case ((ports, signals, variables), p @ DFAny.Port.In()) =>
            (Port(p.name, Port.Dir.In(), Type(p), Init(p)) :: ports, signals, variables)
          case ((ports, signals, variables), p @ DFAny.Port.Out()) =>
            (Port(p.name, Port.Dir.Out(), Type(p), Init(p)) :: ports, signals, variables)
          case ((ports, signals, variables), s : DFAny) if designDB.getConnectionTo(s).isDefined || s.tags.customTags.contains(SyncTag.Reg) =>
            (ports, Signal(s.name, Type(s), Init(s)) :: signals, variables)
          case ((ports, signals, variables), v @ DFAny.Var()) =>
            (ports, signals, Variable(v.name, Type(v), Init(v)) :: variables)
          case (psv, _) => psv
        }
        val entityName = design.designType
        val entity = Entity(entityName, ports)
        val componentInstances = members.collect {
          case x : DFDesign.Block.Internal if x.inlinedRep.isEmpty =>
            val connections = designDB.designMemberTable(x).collect {
              case net : DFNet.Connection if net.hasLateConstruction =>
                val toVal = net.toRef.get
                val fromVal = net.fromRef.get
                if (toVal.isMemberOfDesign(x)) (toVal.name, fromVal.name) else (fromVal.name, toVal.name)
            }
            ComponentInstance(x.name, x.designType, connections)
        }
        val asyncProcess = Process("async_proc", Process.Sensitivity.All(), variables, List())
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