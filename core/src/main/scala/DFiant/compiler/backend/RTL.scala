/*
 *     This file is part of DFiant.
 *
 *     DFiant is free software: you can redistribute it and/or modify
 *     it under the terms of the Lesser GNU General Public License as published by
 *     the Free Software Foundation, either version 3 of the License, or
 *     any later version.
 *
 *     DFiant is distributed in the hope that it will be useful,
 *     but WITHOUT ANY WARRANTY; without even the implied warranty of
 *     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *     Lesser GNU General Public License for more details.
 *
 *     You should have received a copy of the Lesser GNU General Public License
 *     along with DFiant.  If not, see <https://www.gnu.org/licenses/>.
 */

package DFiant
package compiler
package backend

import DFDesign.DB.Patch
import DFiant.DFDesign.DB.Patch.Replace
import DFiant.EdgeDetect.Edge
import constraints.timing.sync._
import ResetParams.{Active, Mode}
import DFiant.sim._

import collection.mutable

final class RTL[D <: DFDesign](c : IRCompilation[D]) {
  private val designDB =
    c.singleStepPrev
     .initCalc
     .explicitNamedVars
     .db

  private val clockParams = {
    import designDB.__getset
    ClockParams.get
  }
  private val resetParams = {
    import designDB.__getset
    ResetParams.get
  }

  //Filter for DB Patch replacement of only assigned values
  private object Assigned extends Replace.RefFilter {
    def apply(refs : Set[DFMember.Ref])(implicit getSet : MemberGetSet) : Set[DFMember.Ref] =
      refs.filter {
        case r : DFMember.OwnedRef => r.refType match {
          case _ : DFNet.ToRef.Type => true
          case _ => false
        }
        case _ => false
      }
  }

  //Filter for DB Patch replacement of only non-assigned values
  private object NotAssigned extends Replace.RefFilter {
    def apply(refs : Set[DFMember.Ref])(implicit getSet : MemberGetSet) : Set[DFMember.Ref] =
      refs -- Assigned(refs)
  }

  private def getClockedDB : (DFDesign.DB, Map[DFDesign.Block, RTL.ClkRstDesign]) = {
    import designDB.__getset
    val addedClkRst : mutable.Map[DFDesign.Block, RTL.ClkRstDesign] = mutable.Map()
    val patchList = designDB.designMemberList.flatMap {case(block, members) =>
      val clockedBlocks = members.collect {
        case b : DFDesign.Block if (addedClkRst.contains(b)) => b
      }
      val hasBlockClk = clockedBlocks.exists{b => addedClkRst(b).hasClk}
      val hasBlockRst = clockedBlocks.exists{b => addedClkRst(b).hasRst}
      lazy val hasPrevClk = members.exists{
        case _ : DFAny.Alias.Prev => true
        case _ => false
      }
      lazy val hasPrevRst = members.exists{
        case p : DFAny.Alias.Prev => p.getInit match {
          case Some(_ :+ _) => true
          case _ => false
        }
        case _ => false
      }
      val topSimulation = block match {
        case DFDesign.Block.Top(_, _, DFSimDesign.Mode.On) => true
        case _ => false
      }
      if (hasBlockClk || hasBlockRst || hasPrevClk || hasPrevRst) {
        val dsnClkRst = new RTL.ClkRstDesign(clockParams, resetParams, topSimulation) {
          if (hasBlockClk || hasPrevClk) clk //touch lazy clock
          if (hasBlockRst || hasPrevRst) rst //touch lazy reset

          //Clock and Reset toggling
          if (topSimulation) {
            val rstInactive = s"'${resetParams.inactiveInt}'"
            vhdl"$clk <= not $clk after 5000 ps;"
            if (hasRst) vhdl"$rst <= $rstInactive after 10000 ps;"
          }
        }
        val connPatchList = clockedBlocks.map {cb =>
          val d = addedClkRst(cb)
          cb -> Patch.Add(new MetaDesign {
            if (d.hasClk) DFNet.Connection(d.clk, dsnClkRst.clk)
            if (d.hasRst) DFNet.Connection(d.rst, dsnClkRst.rst)
          }, Patch.Add.Config.After)
        }

        addedClkRst.update(block, dsnClkRst)
        (members.head -> Patch.Add(dsnClkRst, Patch.Add.Config.Before)) :: connPatchList
      } else None
    }
    (designDB.patch(patchList), addedClkRst.toMap)
  }

  def toRTLForm : IRCompilation[D] = {
    final case class PrevReplacements(
      prevRegDcl : DFAny.Dcl, relVal : DFAny, prevPatch : List[(DFMember, Patch)]
    ) {
      private var sig : DFAny = _
      def sigAssign(implicit ctx : DFBlock.Context) : Unit = sig = (relVal, prevRegDcl) match {
        case (DFAny.In(),_) => relVal
        case _ if !relVal.name.endsWith("_sig") =>
          implicit val __getSet : MemberGetSet = ctx.db.getSet
          val sig = DFAny.NewVar(prevRegDcl.dfType) setName s"${relVal.name}_sig"
          sig.assign(relVal)
          sig
        case _ => relVal
      }
      def rstAssign(implicit ctx : DFBlock.Context) : Unit = relVal.getInit match {
        case Some(i +: _) =>
          val initConst = DFAny.Const.forced(prevRegDcl.dfType, i)
          prevRegDcl.assign(initConst)
        case _ =>
      }
      def clkAssign(implicit ctx : DFBlock.Context) : Unit = prevRegDcl.assign(sig)
    }
    val (clockedDB, addedClkRst) = getClockedDB
    val patchList = clockedDB.designMemberList.flatMap {case(block, members) =>
      import clockedDB.__getset
      var hasPrevRst = false
      //Locating all xyz_prev := xyz.prev assignments
      val prevReplacements : List[PrevReplacements] = members.collect {
        case prevNet @ DFNet.Assignment.Unref(xyz_prev @ DFAny.NewVar(), xyzDOTprev @ DFAny.Alias.Prev.Unref(_, relVal, _, _, _), _, _) =>
          val prevVarInit = relVal.getInit match {
            case Some(i +: _) if !i.isBubble => Some(Seq(i))
            case _ => None
          }
          val prevReg = xyz_prev.copy(externalInit = prevVarInit) !! RTL.Tag.Mod.Reg
          val prevPatch = Assigned(designDB.memberTable(xyz_prev)).size match {
            case 1 => //remove the net
              List(
                prevNet -> Patch.Remove,
                xyz_prev -> Patch.Replace(prevReg, Patch.Replace.Config.FullReplacement),
                xyzDOTprev -> Patch.Replace(prevReg, Patch.Replace.Config.ChangeRefAndRemove)
              )
            case _ =>
              val newVar = xyz_prev.setNameSuffix("_var").copy(externalInit = None)
              List(
                xyz_prev -> Patch.Add(List(prevReg, newVar), Patch.Add.Config.ReplaceWithLast(Patch.Replace.Config.FullReplacement, Assigned)),
                xyz_prev -> Patch.Replace(prevReg, Patch.Replace.Config.ChangeRefAndRemove, NotAssigned),
                xyzDOTprev -> Patch.Replace(prevReg, Patch.Replace.Config.ChangeRefAndRemove)
              )
          }

          xyzDOTprev.getInit match {
            case Some(i +: _) if !i.isBubble => hasPrevRst = true
            case _ =>
          }
          PrevReplacements(prevReg, relVal, prevPatch)
      }
      val topSimulation = block match {
        case DFDesign.Block.Top(_, _, DFSimDesign.Mode.On) => true
        case _ => false
      }
      if (prevReplacements.nonEmpty) {
        val clockedDsn = addedClkRst(block)
        val prevDsn = new RTL.ClkRstDesign(clockParams, resetParams, topSimulation) {
          prevReplacements.foreach(pr => pr.sigAssign)
          private def rstBlock() : Unit = prevReplacements.foreach(pr => pr.rstAssign)
          private def clkBlock() : Unit = prevReplacements.foreach(pr => pr.clkAssign)
          private val clkCond : DFBool = clockParams.edge match {
            case Edge.Rising => clk.rising().anonymize
            case Edge.Falling => clk.falling().anonymize
          }
          if (hasPrevRst) {
            val rstCond : DFBool = resetParams.active match {
              case Active.Low => (rst === 0).anonymize
              case Active.High => (rst === 1).anonymize
            }
            resetParams.mode match {
              case Mode.Async => ifdf(rstCond)(rstBlock()).elseifdf(clkCond)(clkBlock())
              case Mode.Sync => ifdf(clkCond)(ifdf(rstCond)(rstBlock()).elsedf(clkBlock()))
            }
          } else
            ifdf(clk.rising())(clkBlock())
        }

        //replacing the clock and reset with the ones already added in clockedDB
        val clkPatch = (prevDsn.clk -> Patch.Replace(clockedDsn.clk, Patch.Replace.Config.ChangeRefAndRemove))
        val rstPatch = if (hasPrevRst) List(prevDsn.rst -> Patch.Replace(clockedDsn.rst, Patch.Replace.Config.ChangeRefAndRemove)) else List()
        val prevPatchList = clkPatch :: rstPatch
        val prevDB = prevDsn.getDB.patch(prevPatchList)
        //adding the clocked prev "signaling" with the clk-rst guards
        (block -> Patch.Add(prevDB, Patch.Add.Config.Inside)) :: prevReplacements.flatMap(_.prevPatch)
      } else None
    }
    c.newStage(clockedDB.patch(patchList)).clearInitCalc
  }
}

object RTL {
  sealed trait Tag[-T <: DFMember] extends DFMember.CustomTagOf[T]
  object Tag {
    /**
      * Clock
      */
    case object Clk extends Tag[DFAny] {
      override def toString: String = "RTL.Tag.Clk"
    }
    /**
      * Reset
      */
    case object Rst extends Tag[DFAny] {
      override def toString: String = "RTL.Tag.Rst"
    }
    sealed trait Mod extends Tag[DFAny]
    object Mod {
      /**
        * In VHDL: Signal
        *
        * In Verilog: Wire
        */
      case object Wire extends Mod {
        override def toString: String = "RTL.Tag.Mod.Wire"
      }
      /**
        * In VHDL: Signal
        *
        * In Verilog: Reg
        */
      case object Reg extends Mod {
        override def toString: String = "RTL.Tag.Mod.Reg"
      }
      /**
        * In VHDL: Variable
        *
        * In Verilog: Reg
        */
      case object Var extends Mod {
        override def toString: String = "RTL.Tag.Mod.Var"
      }
    }
  }

  object IsReset {
    def unapply(arg: DFAny)(implicit getSet: MemberGetSet): Boolean =
      arg.isTaggedWith(Tag.Rst)
  }
  object IsClock {
    def unapply(arg: DFAny)(implicit getSet: MemberGetSet): Boolean =
      arg.isTaggedWith(Tag.Clk)
  }

  object IfBlock {
    def unapply(
      cb: ConditionalBlock.IfBlock
    )(implicit getSet: MemberGetSet): Option[(DFAny, Boolean)] =
      (cb.condRef.get: DFAny) match {
        case DFAny.Func2.Unref(
        _,
        rst @ IsReset(),
        DFAny.Func2.Op.==,
        DFAny.Const(_, DFBool.Token(_, Some(edge)), _, _),
        _,
        _
        ) =>
          Some(rst, edge)
        case x =>
          x.getOwnerBlock match {
            case DFInlineComponent.Block(
            EdgeDetect.Rep.Unref(clk @ IsClock(), edge)
            ) =>
              Some(clk, edge == EdgeDetect.Edge.Rising)
            case _ => None
          }
      }
  }
  object ElseIfBlock {
    def unapply(
      cb: ConditionalBlock.ElseIfBlock
    )(implicit getSet: MemberGetSet): Option[(DFAny, Boolean)] =
      cb.condRef.get.getOwnerBlock match {
        case DFInlineComponent.Block(
        EdgeDetect.Rep.Unref(clk @ IsClock(), edge)
        ) =>
          Some(clk, edge == EdgeDetect.Edge.Rising)
        case _ => None
      }
  }
  object Net {
    def unapply(net: DFNet)(implicit getSet: MemberGetSet): Boolean =
      net.toRef.get.isTaggedWith(RTL.Tag.Mod.Reg)
  }


  abstract class ClkRstDesign(clkParams : ClockParams, rstParams : ResetParams, simulation : Boolean)(
    implicit ctx : ContextOf[ClkRstDesign]
  ) extends MetaDesign {
    private var _hasClk = false
    private var _hasRst = false
    private val clkInit : Int = clkParams.inactiveInt
    private val rstInit : Int = rstParams.activeInt
    final lazy val clk = {
      _hasClk = true
      if (simulation) DFBit().forcedInit(Seq(DFBool.Token(clkInit))).setName(clkParams.name) !! RTL.Tag.Clk
      else DFBit() <> IN !! RTL.Tag.Clk setName(clkParams.name)
    }
    final lazy val rst = {
      _hasRst = true
      if (simulation) DFBit().forcedInit(Seq(DFBool.Token(rstInit))).setName(rstParams.name) !! RTL.Tag.Rst
      else DFBit() <> IN !! RTL.Tag.Rst setName(rstParams.name)
    }
    final def hasClk : Boolean = _hasClk
    final def hasRst : Boolean = _hasRst
  }

  implicit class Analysis(designDB : DFDesign.DB) {
    import designDB.__getset

    def getSensitivityList(design : DFDesign.Block) : List[String] = {
      val producers = designDB.designMemberTable(design).flatMap {
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
        case v @ DFAny.NewVar() if v.isTaggedWith(RTL.Tag.Mod.Reg) => v
        case v @ DFAny.NewVar() if designDB.getAssignmentsTo(v).isEmpty => v
      }
      signalsOrPorts.map(_.name)
    }
  }
}