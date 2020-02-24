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

package ZFiant
package compiler
package sync

import DFDesign.DB.Patch
import collection.mutable

final class ClockedPrevOps[D <: DFDesign, S <: shapeless.HList](c : Compilable[D, S]) {
  private val designDB = c.singleStepPrev.calcInit.db
  import designDB.__getset

  private def getClockedDB : (DFDesign.DB, Map[DFDesign.Block, ClkRstDesign]) = {
    val addedClkRst : mutable.Map[DFDesign.Block, ClkRstDesign] = mutable.Map()
    val patchList = designDB.designMemberList.flatMap {case(block, members) =>
      val clockedBlocks = members.collect {
        case b : DFDesign.Block if (addedClkRst.contains(b)) => b
      }
      val hasBlockClk = clockedBlocks.exists{b => addedClkRst(b).hasClk}
      val hasBlockRst = clockedBlocks.exists{b => addedClkRst(b).hasRst}
      lazy val hasPrevClk = members.exists{
        case _ : DFAny.Alias.Prev[_] => true
        case _ => false
      }
      lazy val hasPrevRst = members.exists{
        case p : DFAny.Alias.Prev[_] => p.tags.init match {
          case Some(_ :: _) => true
          case _ => false
        }
        case _ => false
      }
      if (hasBlockClk || hasBlockRst || hasPrevClk || hasPrevRst) {
        val dsnClkRst = new ClkRstDesign {
          if (hasBlockClk || hasPrevClk) clk //touch lazy clock
          if (hasBlockRst || hasPrevRst) rst //touch lazy reset
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
    val (clockedDB, addedClkRst) = getClockedDB
    val patchList = clockedDB.designMemberList.flatMap {case(block, members) =>
      var hasPrevRst = false
      val prevTpls = members.collect {
        case p @ DFAny.Alias.Prev(dfType, relValRef, _, ownerRef, tags) =>
          val modifier = tags.init match {
            case Some(i :: _) =>
              hasPrevRst = true
              DFAny.NewVar.Initialized(Seq(i))
            case _ => DFAny.NewVar.Uninitialized
          }
          (p, relValRef.get, DFAny.NewVar(dfType, modifier, ownerRef, tags))
      }
      if (prevTpls.nonEmpty) {
        val clockedDsn = addedClkRst(block)
        val prevDsn = new ClkRstDesign {
          private def rstBlock : Unit = prevTpls.foreach {
            case (_, _, prevVar) => prevVar.tags.init match {
              case Some(i :: _) =>
                val initConst = DFAny.Const(prevVar.dfType, i)
                prevVar.assign(initConst)
              case _ =>
            }
          }
          private def clkBlock : Unit = prevTpls.foreach {
            case (_, rv, prevVar) =>
              prevVar.assign(rv)
          }
          if (hasPrevRst)
            ifdf(!rst)(rstBlock).elseifdf(clk.rising())(clkBlock)
          else
            ifdf(clk.rising())(clkBlock)
        }
        //replacing the clock and reset with the ones already added in clockedDB
        val clkPatch = (prevDsn.clk -> Patch.Replace(clockedDsn.clk, Patch.Replace.Config.ChangeRefAndRemove))
        val rstPatch = if (hasPrevRst) List(prevDsn.rst -> Patch.Replace(clockedDsn.rst, Patch.Replace.Config.ChangeRefAndRemove)) else List()
        val prevPatchList = clkPatch :: rstPatch
        val prevDB = prevDsn.getDB.patch(prevPatchList)
        //adding the clocked prev "signaling" with the clk-rst guards
        (block -> Patch.Add(prevDB, Patch.Add.Config.Inside)) ::
        prevTpls.map{case (p, _, prevVar) => p -> Patch.Replace(prevVar, Patch.Replace.Config.FullReplacement)}
      } else None
    }
    c.newStage[ClockedPrev](clockedDB.patch(patchList), Seq())
  }
}

object ClockedPrevOps {
  implicit def evClockedPrevOps[D <: DFDesign, S <: shapeless.HList, C](c : C)(implicit conv : C => Compilable[D, S])
  : ClockedPrevOps[D, S] = new ClockedPrevOps[D, S](c)
}

trait ClockedPrev extends Compilable.Stage

