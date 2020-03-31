package DFiant
package compiler

import DFDesign.DB.Patch
import DFiant.compiler.sync.Sync

/*
Internal design blocks will be connected via dedicated "wiring" variables.
This is very useful when compiling to the basic RTL languages (VHDL/Verilog) that require extra signaling
and cannot directly connect between sibling design blocks or use output ports. So each port PPP for block BBB
becomes a connection PPP.BBB <> PPP_BBB, where the extra variable `PPP_BBB` is used to represent an RTL
signal. This of course does not change the correctness of any DFiant design, but is required for later phase
where the code is compiled to RTL.

For example:
  trait ID extends DFDesign {
    val i = DFUInt(8) <> IN
    val o = DFUInt(8) <> OUT
    o <> i
  }

  trait IDTop extends DFDesign {
    val x = DFUInt(8) <> IN
    val y = DFUInt(8) <> OUT
    val id1 = new ID {}
    val id2 = new ID {}
    id1.i <> x
    id1.o <> id2.i //This cannot be done in VHDL/Verilog
    id2.o <> y
  }

  Will become:
  trait IDTop extends DFDesign {
    final val x = DFUInt(8) <> IN
    final val y = DFUInt(8) <> OUT
    final val id1_i = DFUInt(8)
    final val id1_o = DFUInt(8)
    final val id1 = new ID {
      i <> id1_i
      id1_o <> o
    }
    final val id2_i = DFUInt(8)
    final val id2_o = DFUInt(8)
    final val id2 = new ID {
      i <> id2_i
      id2_o <> o
    }
    id1_i <> x
    id2_i <> id1_o
    y <> id2_o
  }
*/
final class ViaPortConnectionOps[D <: DFDesign, S <: shapeless.HList](c : Compilable[D, S]) {
  private val designDB = c.db
  def viaPortConnection = {
    val internalBlocks : List[DFDesign.Block.Internal] = designDB.members.collect {
      case d : DFDesign.Block.Internal if d.inlinedRep.isEmpty => d
    }
    val patchList : List[(DFMember, Patch)] = internalBlocks.flatMap{ib =>
      //getting only ports that are not already connected to variables unless these are clock variables
      val ports : List[DFAny] = designDB.designMemberTable(ib).flatMap {
        case p @ DFAny.Port.Out() =>
//          val conns = designDB.getConnectionFrom(p)
//          conns.headOption match {
//            case Some(DFAny.NewVar()) if conns.size == 1 => None
//            case _ => Some(p)
//          }
          Some(p)
        case p @ DFAny.Port.In() =>
//          import designDB.__getset
//          designDB.getConnectionTo(p) match {
//            case Some(v @ DFAny.NewVar()) if v.tags.customTags.exists{case _ : Sync.Tag => true} => Some(p)
//            case Some(v @ DFAny.NewVar()) => None
//            case Some(_ @ DFAny.Port.In()) => Some(p)
//            case Some(x) if x.isMemberOfDesign(ib) || x.isMemberOfDesign(ib.getOwnerDesign) => None
//            case _ => Some(p)
//          }
          Some(p)
        case _ => None
      }
      //Meta design to construct the variables to be connected to the ports
      val addVarsDsn = new MetaDesign() {
        val portsToVars : List[(DFAny, DFAny)] = ports.map {p =>
          p -> (DFAny.NewVar(p.dfType) setName(s"${ib.name}_${p.name}"))
        }
      }
      //Meta design for connections between ports and the added variables
      val connectDsn = new MetaDesign(true) {
        val refPatches : List[(DFMember, Patch)] = addVarsDsn.portsToVars.map {case (p, v) =>
          p match {
            case _ @ DFAny.Port.Out() => DFNet.Connection(v, p)
            case _ @ DFAny.Port.In() => DFNet.Connection(p, v)
            case _ => ???
          }
          (p, Patch.Replace(v, Patch.Replace.Config.ChangeRefOnly, Patch.Replace.Scope.Outside(ib)))
        }
      }
      (ib -> Patch.Add(addVarsDsn, Patch.Add.Config.Before)) ::
        (ib -> Patch.Add(connectDsn, Patch.Add.Config.Inside)) :: connectDsn.refPatches
    }
    c.newStage[ViaPortConnection](designDB.patch(patchList), Seq())
  }
}

trait ViaPortConnection extends Compilable.Stage