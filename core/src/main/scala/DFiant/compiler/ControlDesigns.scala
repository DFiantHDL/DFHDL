package DFiant
package compiler

import DFDesign.DB.Patch
import DFiant.internals.SetOps
import analysis._

import scala.annotation.tailrec
import scala.collection.mutable

/*
The design are controlled through the Control construct for disabling/clearing/enabling the state (change).
During this stage we do the following:
1. Detect all the designs that require control. Those are:
  *  Designs are being controlled via `.enable`, `.disable`, `.clear` or they are member designs of
     controlled designs. If a design is always enabled, the control mechanism is ignored.
  AND
  *  The designs have state variables.
2. For all controlled designs:
   A. We add a control enumeration input port and connect it to a control variable at the owner.
   B. Replace each control construct with assignment to the control variable.
   C. Add control logic for all the state variables and controllable member designs.

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
    val acc = new Accumulator
    acc.disable()
    ifdf (x < 10) {
      acc.enable()
      acc.x <> x
      y := acc.o
    }
  }

  Will become:
  @df class Accumulator extends DFDesign {
    final val x     = DFUInt(8) <> IN
    final val o     = DFUInt(8) <> OUT
    final val dsnCtrl = DFEnum(Control.Op)
    private val acc = DFUInt(8) init 0
    acc := acc + x
    o := acc.prev
    matchdf(dsnCtrl)
     .casedf(Control.Op.Disable){acc := acc.prev}
     .casedf_{}
  }

  @df class AccumulateSmallerThan10 extends DFDesign {
    val x = DFUInt(8) <> IN
    val y = DFUInt(8) <> OUT
    val acc = new Accumulator
    val acc_dsnCtrl = DFEnum(Control.Op)
    acc.dsnCtrl `<LZ>` acc_dsnCtrl
    acc_dsnCtrl := Control.Op.Disable
    ifdf (x < 10) {
      acc_dsnCtrl := Control.Op.Enable
      acc.x <> x
      y := acc.o
    }
  }
*/
import DFDesign.Control.Op
final class ControlDesigns[D <: DFDesign](c : IRCompilation[D]) {
  private val designDB = c.db
  import designDB.__getset
  private val verbose : Boolean = false
  private def verbose(block : => Unit) : Unit = {
    if (verbose) block
  }
  def controlDesigns : IRCompilation[D] = {
    val controls = designDB.members.collect {
      case c : DFDesign.Control => c
    }
    //controlled designs are designs that are being controlled by a control construct
    val controlledDesigns : Set[DFDesign.Block] =
      controls.view.map(_.designRef.get).toSet

    verbose {
      println("controlledDesigns:")
      controlledDesigns.foreach(c => println(c.getFullName))
    }

    val controllingDesigns : Set[DFDesign.Block] =
      controls.view.map(_.getOwnerDesign).toSet

    verbose {
      println("controllingDesigns:")
      controllingDesigns.foreach(c => println(c.getFullName))
    }

    //state designs are designs that have state variables
    val stateDesigns : Map[DFDesign.Block, Set[DFAny.Dcl]] = ???
//      designDB.dependencyContext.stateVars.groupBy(_.getOwnerDesign)

    verbose {
      println("stateDesigns:")
      stateDesigns.foreach(c => println(c._1.getFullName))
    }

    @tailrec def getControlledOwners(current : DFDesign.Block, list : List[DFDesign.Block]) : List[DFDesign.Block] = {
      if (controllingDesigns.contains(current)) list
      else if (current.isTop) Nil
      else getControlledOwners(current.getOwnerDesign, current :: list)
    }
    //controlled state designs are controlled designs that have controllable designs members at any hierarchy
    val controlledStateDesigns : Set[DFDesign.Block] =
      stateDesigns.view.keys.flatMap(d => getControlledOwners(d, List(d))).toSet

    verbose {
      println("controlledStateDesigns:")
      controlledStateDesigns.foreach(c => println(c.getFullName))
    }

    //save control ports for all designs that have it
    val controlPortMap = mutable.Map.empty[DFDesign.Block, DFAny.DclOf[DFEnum.Type[Op.type]]]
    //save control vars for all designs that have it
    val controlVarMap = mutable.Map.empty[DFDesign.Block, DFAny.VarOf[DFEnum.Type[Op.type]]]

    val patchList : List[(DFMember, Patch)] = designDB.designMemberList.flatMap {
      case (ownerDesign, members) if controlledStateDesigns.contains(ownerDesign) || controllingDesigns.contains(ownerDesign) =>
        verbose {
          println("-----------------")
          println("handling", ownerDesign.getFullName)
        }
        val stateVarSet = stateDesigns.getOrElse(ownerDesign, Set())
        //Ordered list of state variables
        //We use a list so the compilation output will remain consistent at every execution
        val stateVarList = members.collect{case v : DFAny.Dcl if stateVarSet.contains(v) => v}
        val controlledStateDesignMembers = members.collect{case d : DFDesign.Block if controlledStateDesigns.contains(d) => d}
        val controlledDesignMembers = members.collect{case d : DFDesign.Block if controlledDesigns.contains(d) => d}
        val controllers = members.collect{case c : DFDesign.Control => c}

        //Adding control port for all designs that require it
        val controlPortPatchOption = if (controlledStateDesigns.contains(ownerDesign)) {
          verbose{println(s"Adding ${ownerDesign.getFullName} dsnCtrl port")}
          val dsn = new MetaDesign() {
            val dsnCtrl = DFEnum(Op) <> IN
            controlPortMap += (ownerDesign -> dsnCtrl)
          }
          Some(ownerDesign -> Patch.Add(dsn, Patch.Add.Config.InsideFirst))
        } else None

        //Handling controlled design members
        val designMemberPatchList = controlledStateDesignMembers.map {
          //The design is directly controlled by its owner, so we need to create a variable at the owner
          //that connects (lazily) to the input control port. That variable will be assigned by the owner either
          //where the control takes place or when the control port of the owner is not enabled and therefore
          //forces this design's control accordingly.
          case md if controlledDesigns.contains(md) =>
            verbose{println(s"Adding ${md.getFullName}_dsnCtrl_var")}
            val dsn = new MetaDesign() {
              val dsnCtrl = DFEnum(Op) <> VAR setName s"${md.name}_dsnCtrl_var"
              dsnCtrl := Op.Enable //default
              DFNet.LazyConnection(controlPortMap(md), dsnCtrl)
              controlVarMap += (md -> dsnCtrl)
            }
            md -> Patch.Add(dsn, Patch.Add.Config.After)
          //The design is not directly controlled by its owner, so we just need to connect the control ports
          case md =>
            verbose{println(s"Connecting ${md.getFullName}.dsnCtrl <> ${ownerDesign.getFullName}.dsnCtrl")}
            val dsn = new MetaDesign() {
              DFNet.Connection(controlPortMap(md), controlPortMap(ownerDesign))
            }
            md -> Patch.Add(dsn, Patch.Add.Config.After)
        }

        //We convert all control constructs to assignment to the relevant control variable or
        //removing them if they are redundant where there is no state to control
        val controlPatchList = controllers.map { control =>
          controlVarMap.get(control.designRef.get) match {
            case Some(dsnCtrl) =>
              verbose{println(s"Converting control ${control.op}")}
              val dsn = new MetaDesign() {
                dsnCtrl := control.op
              }
              control -> Patch.Add(dsn, Patch.Add.Config.ReplaceWithLast())
            case None =>
              control -> Patch.Remove
          }
        }

        val statePatchOption = controlPortMap.get(ownerDesign) match {
          case Some(dsnCtrl) if stateVarList.nonEmpty || controlledDesignMembers.nonEmpty =>
            verbose {
              println(s"Handling state vars:", stateVarSet.map(_.name))
              println(s"Handling control vars for:", controlledDesignMembers.map(_.name))
            }
            val dsn = new MetaDesign() {
              matchdf(dsnCtrl)
                //To disable state variables, we force them to keep their state
                .casedf(Op.Stall) {
                  stateVarList.foreach(v => v.asVar := v.asVal.prev)
                  controlledDesignMembers.foreach(controlVarMap(_) := Op.Stall)
                }
                //To clear state variables, we assign their initial value
                .casedf(Op.Init) {
                  stateVarList.foreach {v =>
                    v.externalInit match {
                      case Some(token +: _) => v.asVar := DFAny.Const.forced[DFAny.Type](token)
                      case _ => v.asVar := ?
                    }
                  }
                  controlledDesignMembers.foreach(controlVarMap(_) := Op.Init)
                }
                //The defaults take care of the enable state, so we do nothing here
                .casedf(Op.Enable) {/* do nothing */}
            }
            Some(ownerDesign -> Patch.Add(dsn, Patch.Add.Config.InsideLast))
          case _ => None
        }
        List(
          controlPortPatchOption,
          designMemberPatchList,
          controlPatchList,
          statePatchOption
        ).flatten
      case _ => Nil
    }
    c.newStage(designDB.patch(patchList)).printCodeString
  }
}