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

final class ClockedPrevOps[D <: DFDesign, S <: shapeless.HList](c : Compilable[D, S]) {
  private val designDB = c.singleStepPrev.calcInit.db
  import designDB.__getset



  def clockedPrev = {
    val patchList = designDB.designMemberList.flatMap {case(block, members) =>
      val prevTpls = members.collect {
        case p @ DFAny.Alias.Prev(dfType, relValRef, step, ownerRef, tags) =>
          val modifier = tags.init match {
            case Some(i :: _) => DFAny.NewVar.Initialized(Seq(i))
            case _ => DFAny.NewVar.Uninitialized
          }
          (p, relValRef.get, DFAny.NewVar(dfType, modifier, ownerRef, tags))
      }
      if (prevTpls.nonEmpty) {
        val dsn = new MetaDesign() {
          val clk = DFBool() <> IN
          val rst = DFBool() <> IN

          ifdf(rst === true) {
            prevTpls.foreach {
              case (_, _, prevVar) => prevVar.tags.init match {
                case Some(i :: _) =>
                  val initConst = DFAny.Const(prevVar.dfType, i)
                  prevVar.assign(initConst)
                case _ =>
              }
            }
          }.elseifdf(clk.rising) {
            prevTpls.foreach {
              case (p, rv, prevVar) =>
                prevVar.assign(rv)
            }
          }
        }
        members(1) -> Patch.Add(dsn, Patch.Add.Config.Before) ::
          prevTpls.map{case (p, _, prevVar) => p -> Patch.Replace(prevVar, Patch.Replace.Config.FullReplacement)}
      } else None
    }
    c.newStage[ClockedPrev](designDB.patch(patchList), Seq())
  }
}

object ClockedPrevOps {
  implicit def evClockedPrevOps[D <: DFDesign, S <: shapeless.HList, C](c : C)(implicit conv : C => Compilable[D, S])
  : ClockedPrevOps[D, S] = new ClockedPrevOps[D, S](c)
}

trait ClockedPrev extends Compilable.Stage

