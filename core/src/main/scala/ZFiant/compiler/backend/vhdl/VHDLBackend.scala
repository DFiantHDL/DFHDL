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
     .viaPortConnection
     .uniqueDesigns
     .uniqueNames(reservedKeywords, caseSensitive = false)
     .clockedPrev
     .db

  import designDB.__getset

  def compile = {
    val designTypes = mutable.Set.empty[String]
    val files = designDB.designMemberList.flatMap {
      case (design : DFDesign.Block.Internal, _) if design.inlinedRep.nonEmpty => None
      case (design, members) if !designTypes.contains(design.designType) =>
        designTypes += design.designType
        val ports = members.collect {
          case p @ DFAny.Port.In() => Port(p.name, Port.Dir.In(), Type(p), Init(p))
          case p @ DFAny.Port.Out() => Port(p.name, Port.Dir.Out(), Type(p), Init(p))
        }
        val entityName = design.designType
        val entity = Entity(entityName, ports)
        val signals = members.flatMap {
          case DFAny.Port.In() | DFAny.Port.Out() => None
          case x : DFAny if designDB.getConnectionTo(x).isDefined || x.tags.customTags.contains(SyncTag.Reg) =>
            Some(Signal(x.name, Type(x), Init(x)))
          case _ => None
        }
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
        val architecture = Architecture(s"${entityName}_arch", entityName, signals, componentInstances)
        val file = File(entity, architecture)
        Some(Compilable.Cmd.GenFile(s"${design.designType}.vhdl", s"$file"))
      case _ => None
    }
    c.newStage[VHDLCompiler](designDB, c.cmdSeq ++ files)
  }
}

trait VHDLCompiler extends Compilable.Stage