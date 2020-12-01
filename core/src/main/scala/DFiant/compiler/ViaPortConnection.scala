package DFiant
package compiler

import DFDesign.DB.Patch

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
final class ViaPortConnection[D <: DFDesign](c : IRCompilation[D]) {
  private val designDB = c.db
  import designDB.__getset
  def viaPortConnection : IRCompilation[D] = {
    val internalBlocks : List[DFDesign.Block.Internal] = designDB.members.collect {
      case d : DFDesign.Block.Internal if d.inlinedRep.isEmpty => d
    }
    val patchList : List[(DFMember, Patch)] = internalBlocks.flatMap{ib =>
      //getting only ports that are not already connected to variables unless these are clock variables
      val (ports, nets) : (List[DFAny.Member], List[DFNet]) =
        designDB.designMemberTable(ib).foldRight((List.empty[DFAny.Member], List.empty[DFNet])) {
          case (p @ DFAny.Port.Out(), (ports, nets)) =>
            val conns = designDB.getConnectionFrom(p)
            conns.headOption match {
              case Some(n) if n.hasLateConstruction => (ports, nets) //already has via connections
              case Some(n @ DFNet.Connection(DFAny.NewVar(), _, _, _)) if conns.size == 1 =>
                (ports, n :: nets)
              case _ => (p :: ports, nets)
            }
          case (p @ DFAny.Port.In(), (ports, nets)) =>
            designDB.getConnectionTo(p) match {
              //we have a single net that is assigned not more than once
              //(otherwise, for RTL purposes we require another value so an internal multi-assignment rtl variable/reg
              //can be assigned into a signal/wire)
              case Some(n) if n.hasLateConstruction => (ports, nets) //already has via connections
              case Some(n @ DFNet.Connection(_, v @ DFAny.NewVar(), _, _)) if designDB.getAssignmentsTo(v).isEmpty =>
                (ports, n :: nets)
              case _ => (p :: ports, nets)
            }
          case (_, x) => x
        }
      //Meta design to construct the variables to be connected to the ports
      val addVarsDsn = new MetaDesign() {
        val portsToVars : List[(DFAny.Member, DFAny.Member)] = ports.map {p =>
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
          (p, Patch.Replace(v, Patch.Replace.Config.ChangeRefOnly, Patch.Replace.RefFilter.Outside(ib)))
        }
        val movedNets : List[(DFMember, Patch)] = nets.map {n =>
          plantMember(n.setTags(_.setLateConstruction(true))(designDB.__getset)) //planet the net with a
          (n -> Patch.Remove)
        }
      }
      (ib -> Patch.Add(addVarsDsn, Patch.Add.Config.Before)) ::
        (ib -> Patch.Add(connectDsn, Patch.Add.Config.InsideLast)) :: connectDsn.refPatches ++ connectDsn.movedNets
    }
    c.newStage(designDB.patch(patchList))
  }
}