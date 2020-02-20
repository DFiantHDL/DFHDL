///*
// *     This file is part of DFiant.
// *
// *     DFiant is free software: you can redistribute it and/or modify
// *     it under the terms of the Lesser GNU General Public License as published by
// *     the Free Software Foundation, either version 3 of the License, or
// *     any later version.
// *
// *     DFiant is distributed in the hope that it will be useful,
// *     but WITHOUT ANY WARRANTY; without even the implied warranty of
// *     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// *     Lesser GNU General Public License for more details.
// *
// *     You should have received a copy of the Lesser GNU General Public License
// *     along with DFiant.  If not, see <https://www.gnu.org/licenses/>.
// */
//
//package ZFiant
//package compiler
//package sync
//
//import DFDesign.DB.Patch
//
//final class AddClkRstOps[D <: DFDesign, S <: shapeless.HList](c : Compilable[D, S]) {
//  private val designDB = c.singleStepPrev.calcInit.db
//  import designDB.__getset
//
//  def addClkRst = {
//    val patchList = designDB.designMemberList.flatMap {case(block, members) =>
//      val blockClkRst = members.flatMap {
//        case b : DFDesign.Block =>
//          val internalMembers = designDB.designMemberTable(block)
//          val clk = internalMembers.collectFirst{case m if m.tags.customTags.contains(SyncCustomTag.Clock) => m}
//          val rst = internalMembers.collectFirst{case m if m.tags.customTags.contains(SyncCustomTag.Clock) => m}
//          if (clk.nonEmpty || rst.nonEmpty) Some(b, clk, rst)
//          else None
//        case _ => None
//      }
//      val prevTpls = members.collect {
//        case p @ DFAny.Alias.Prev(dfType, relValRef, _, ownerRef, tags) =>
//          val modifier = tags.init match {
//            case Some(i :: _) => DFAny.NewVar.Initialized(Seq(i))
//            case _ => DFAny.NewVar.Uninitialized
//          }
//          (p, relValRef.get, DFAny.NewVar(dfType, modifier, ownerRef, tags))
//      }
//      if (prevTpls.nonEmpty || clockedBlocks.nonEmpty) {
//        val dsn = new ClkRstDesign {
//          if (prevTpls.nonEmpty) {
//            ifdf(!rst) {
//              prevTpls.foreach {
//                case (_, _, prevVar) => prevVar.tags.init match {
//                  case Some(i :: _) =>
//                    val initConst = DFAny.Const(prevVar.dfType, i)
//                    prevVar.assign(initConst)
//                  case _ =>
//                }
//              }
//            }.elseifdf(clk.rising()) {
//              prevTpls.foreach {
//                case (p, rv, prevVar) =>
//                  prevVar.assign(rv)
//              }
//            }
//          }
//          clockedBlocks.foreach {
//            cb =>
//              val d = addedClkRst(cb)
//              DFNet.Connection(d.clk, clk)
//              DFNet.Connection(d.rst, rst)
//          }
//        }
//        addedClkRst.update(block, dsn)
////        (members.head -> Patch.Add(clkDsn, Patch.Add.Config.Before)) ::
//        (block -> Patch.Add(dsn.getDB, Patch.Add.Config.Inside)) ::
//        prevTpls.map{case (p, _, prevVar) => p -> Patch.Replace(prevVar, Patch.Replace.Config.FullReplacement)}
//      } else None
//    }
//    c.newStage[AddClkRst](designDB.patch(patchList), Seq())
//  }
//}
//
//object AddClkRstOps {
//  implicit def evAddClkRstOps[D <: DFDesign, S <: shapeless.HList, C](c : C)(implicit conv : C => Compilable[D, S])
//  : AddClkRstOps[D, S] = new AddClkRstOps[D, S](c)
//}
//
//trait AddClkRst extends Compilable.Stage
//
