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
package sync

import DFDesign.DB.Patch
import DFiant.EdgeDetect.Edge
import DFiant.compiler.sync.ResetParams.{Active, Mode}
import DFiant.sim._
import collection.mutable

final class ClockedPrevOps[D <: DFDesign, S <: shapeless.HList](c : IRCompilation[D, S]) {
  private val designDB = c.singleStepPrev.calcInit.explicitNamedVars.db

  private val clockParams = {
    import designDB.__getset
    ClockParams.get
  }
  private val resetParams = {
    import designDB.__getset
    ResetParams.get
  }

  private def getClockedDB : (DFDesign.DB, Map[DFDesign.Block, ClkRstDesign]) = {
    import designDB.__getset
    val addedClkRst : mutable.Map[DFDesign.Block, ClkRstDesign] = mutable.Map()
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
        case p : DFAny.Alias.Prev => p.tags.init match {
          case Some(_ :: _) => true
          case _ => false
        }
        case _ => false
      }
      val topSimulation = block match {
        case DFDesign.Block.Top(_, _, DFSimDesign.Mode.On) => true
        case _ => false
      }
      if (hasBlockClk || hasBlockRst || hasPrevClk || hasPrevRst) {
        val dsnClkRst = new ClkRstDesign(clockParams, resetParams, topSimulation) {
          if (hasBlockClk || hasPrevClk) clk //touch lazy clock
          if (hasBlockRst || hasPrevRst) rst //touch lazy reset

          //Clock and Reset toggling
          if (topSimulation) {
            val inactiveVHDL = resetParams.active match {
              case Active.Low => "'1'"
              case Active.High => "'0'"
            }
            vhdl"$clk <= not $clk after 5000 ps;"
            if (hasRst) vhdl"$rst <= $inactiveVHDL after 10000 ps;"
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

  def clockedPrev = {
    final case class PrevReplacements(
      prevNet : DFNet, prevVar : DFAny.Dcl, prevVal : DFAny.Alias.Prev, relVal : DFAny, regVar : DFAny.Dcl
    ) {
      private var sig : DFAny = _
      def sigAssign(implicit ctx : DFBlock.Context) : Unit = sig = (relVal, regVar) match {
        case (DFAny.In(),_) => relVal
        case _ if !relVal.name.endsWith("_sig") =>
          implicit val __getSet : MemberGetSet = ctx.db.getSet
          val sig = DFAny.NewVar(regVar.dfType) setName s"${relVal.name}_sig"
          sig.assign(relVal)
          sig
        case _ => relVal
      }
      def rstAssign(implicit ctx : DFBlock.Context) : Unit = relVal.tags.init match {
        case Some(i +: _) =>
          val initConst = DFAny.Const.forced(regVar.dfType, i)
          regVar.assign(initConst)
        case _ =>
      }
      def clkAssign(implicit ctx : DFBlock.Context) : Unit = regVar.assign(sig)
    }
    val (clockedDB, addedClkRst) = getClockedDB
    val patchList = clockedDB.designMemberList.flatMap {case(block, members) =>
      import designDB.__getset
      var hasPrevRst = false
      val prevReplacements : List[PrevReplacements] = members.collect {
        case n @ DFNet.Assignment.Unref(prevVar @ DFAny.NewVar(), prevVal @ DFAny.Alias.Prev(_, relValRef, _, _, tags), _, _) =>
          tags.init match {
            case Some(i +: _) if !i.isBubble => hasPrevRst = true
            case _ =>
          }
          PrevReplacements(n, prevVar, prevVal, relValRef.get, prevVar.setNameSuffix("_reg") !! Sync.Tag.Reg)
      }
      val topSimulation = block match {
        case DFDesign.Block.Top(_, _, DFSimDesign.Mode.On) => true
        case _ => false
      }
      if (prevReplacements.nonEmpty) {
        val clockedDsn = addedClkRst(block)
        val prevDsn = new ClkRstDesign(clockParams, resetParams, topSimulation) {
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
        (block -> Patch.Add(prevDB, Patch.Add.Config.Inside)) ::
          prevReplacements.flatMap {
            case PrevReplacements(prevNet, prevVar, prevVal, relVal, regVar) =>
              val prevVarNoInit = prevVar.copy(externalInit = None).setTags(t => t.copy(init = None))
              List (
                prevVar -> Patch.Add(List(prevVarNoInit, regVar), Patch.Add.Config.ReplaceWithFirst()),
                prevVal -> Patch.Replace(regVar, Patch.Replace.Config.ChangeRefAndRemove),
              )
          }
      } else None
    }
    c.newStage[ClockedPrev](clockedDB.patch(patchList))
  }
}


trait ClockedPrev extends Compilation.Stage

