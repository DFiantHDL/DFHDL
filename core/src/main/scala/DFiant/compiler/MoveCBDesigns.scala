package DFiant
package compiler

import DFDesign.DB.Patch
import analysis._

/*
Designs that are inside control blocks will be moved outside any control block and use wiring to connect.
When a design is moved outside a control block it behaves according to the OutsideOwner behavior tag.
A design can either be disabled, cleared, or enabled outside. The tagging will be converted to the appropriate
control logic.

For example:
  @df class Accumulator extends DFDesign {
    final val x     = DFUInt(8) <> IN
    final val o     = DFUInt(8) <> OUT
    private val acc = DFUInt(8) init 0
    acc := acc + x
    o := acc.prev
  }

  @df class AccumulateSmallerThan10 extends DFDesign {
    val x = DFUInt(8) <> IN
    val y = DFUInt(8) <> OUT
    ifdf (x < 10) {
      val acc = new Accumulator !! OutsideOwnerDisable
      acc.x <> x
      y := acc.o
    }
  }

  Will become:
  @df class AccumulateSmallerThan10 extends DFDesign {
    val x = DFUInt(8) <> IN
    val y = DFUInt(8) <> OUT
    val acc = new Accumulator
    acc.disable()
    ifdf (x < 10) {
      acc.enable()
      acc.x <> x
      y := acc.o
    }
  }
 */
final class MoveCBDesigns[D <: DFDesign](c: IRCompilation[D]) {
  private val designDB = c.db
  import designDB.__getset
  def moveCBDesigns: IRCompilation[D] = {
    val patchList: List[(DFMember, Patch)] = designDB.members.flatMap {
      case d @ DFDesign.Block.Internal
            .Unref(_, cb: DFConditional.Block, _, None) =>
        val topConditionalMember = cb.getTopConditionalMember
        val outsideOp            = d.getTagOf[OutsideOwner].getOrElse(OutsideOwnerInit).op
        //for adding control after the new design position
        val outsideCBDsn = new MetaDesign() {
          DFDesign.Control(d, outsideOp)
        }
        //For adding enable control where the design used to be
        val insideCBDsn = new MetaDesign() {
          DFDesign.Control(d, DFDesign.Control.Op.Enable)
        }
        val inputPatchList = designDB.designMemberTable(d).flatMap {
          case port @ DFAny.Port.In() =>
            designDB.getConnectionTo(port) match {
              case Some(conn @ DFNet.Connection(_, connValue, _, _)) =>
                val outsideCBDsn = new MetaDesign() {
                  final val portVar = DFAny
                    .NewVar(port.dfType)
                    .setName(s"${d.name}_${port.name}_var")
                  portVar := ?
                  DFNet.LazyConnection(port, portVar).anonymize
                }
                val insideCBDsn = new MetaDesign() {
                  DFNet.Assignment(outsideCBDsn.portVar, connValue).anonymize
                }
                List(
                  topConditionalMember -> Patch
                    .Add(outsideCBDsn, Patch.Add.Config.Before),
                  port -> Patch.Replace(
                    outsideCBDsn.portVar,
                    Patch.Replace.Config.ChangeRefOnly,
                    Patch.Replace.RefFilter.Outside(d)
                  ),
                  conn -> Patch
                    .Add(insideCBDsn, Patch.Add.Config.ReplaceWithLast())
                )
            }
          case _ => Nil
        }

        List(
          topConditionalMember -> Patch.Move(d, Patch.Move.Config.Before),
          topConditionalMember -> Patch
            .Add(outsideCBDsn, Patch.Add.Config.Before),
          d -> Patch.Add(insideCBDsn, Patch.Add.Config.After)
        ) ++ inputPatchList
      case _ => Nil
    }
    c.newStage(designDB.patch(patchList)) //.printCodeString
  }
}
