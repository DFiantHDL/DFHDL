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
import ZFiant.EdgeDetect.Edge
import ZFiant.compiler.sync.ResetParams.{Active, Mode}

import collection.mutable

final class ClockedPrevOps[D <: DFDesign, S <: shapeless.HList](c : Compilable[D, S]) {
  private val designDB = c.singleStepPrev.calcInit.db
  import designDB.__getset

  private val clockParams = ClockParams.get
  private val resetParams = ResetParams.get

  private def getClockedDB : (DFDesign.DB, Map[DFDesign.Block, ClkRstDesign]) = {
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
      if (hasBlockClk || hasBlockRst || hasPrevClk || hasPrevRst) {
        val dsnClkRst = new ClkRstDesign(clockParams.name, resetParams.name) {
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
      val prevTpls : List[(DFAny, DFAny, DFAny.VarOf[DFAny.Type])] = members.collect {
        case p @ DFAny.Alias.Prev(dfType, relValRef, _, ownerRef, tags) =>
          val externalInit = tags.init match {
            case Some(i :: _) =>
              hasPrevRst = true
              Some(Seq(i))
            case _ => None
          }
          (p, relValRef.get, DFAny.Dcl(dfType, DFAny.Modifier.NewVar, externalInit, ownerRef, tags !! Sync.Tag.Reg).asInstanceOf[DFAny.VarOf[DFAny.Type]])
      }
      if (prevTpls.nonEmpty) {
        val clockedDsn = addedClkRst(block)
        val prevDsn = new ClkRstDesign(clockParams.name, resetParams.name) {
          val sigs : List[DFAny] = prevTpls.map {
            case (_,rv @ DFAny.In(), _) => rv
            case (_,rv,prevVar) if !rv.name.endsWith("_sig") =>
              val sig = DFAny.NewVar(prevVar.dfType) setName s"${rv.name}_sig"
              sig.assign(rv)
              sig
            case (_, rv, _) => rv
          }
          private def rstBlock : Unit = prevTpls.foreach {
            case (_, _, prevVar) => prevVar.tags.init match {
              case Some(i :: _) =>
                val initConst = DFAny.Const.forced(prevVar.dfType, i)
                prevVar.assign(initConst)
              case _ =>
            }
          }
          private def clkBlock : Unit = (prevTpls lazyZip sigs).foreach {
            case ((_, _, prevVar), sig) => prevVar.assign(sig)
          }
          val clkCond : DFBool = clockParams.edge match {
            case Edge.Rising => clk.rising().anonymize
            case Edge.Falling => clk.falling().anonymize
          }
          if (hasPrevRst) {
            val rstCond : DFBool = resetParams.active match {
              case Active.Low => (rst === 0).anonymize
              case Active.High => (rst === 1).anonymize
            }
            resetParams.mode match {
              case Mode.Async => ifdf(rstCond)(rstBlock).elseifdf(clkCond)(clkBlock)
              case Mode.Sync => ifdf(clkCond)(ifdf(rstCond)(rstBlock).elsedf(clkBlock))
            }
          } else
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
  def setClock(clkName : String) : Compilable[D, S] = ???
}


trait ClockedPrev extends Compilable.Stage

