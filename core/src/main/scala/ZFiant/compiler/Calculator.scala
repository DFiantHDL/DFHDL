package ZFiant
package compiler

import DFDesign.DB.Patch
import scala.annotation.tailrec

//final class Calculator[C](c : C)(implicit comp : Compilable[C]) {
//  private val designDB = comp(c)
//  import designDB.getset
//  @tailrec private def calcInit(remaining : List[DFAny], calc : Map[DFAny, Seq[DFAny.Token]], requestedCalc : Set[DFAny]) : Map[DFAny, Seq[DFAny.Token]] = {
//    def getInit[T <: DFAny](member : T) : Option[Seq[member.TToken]] = member.tags.init match {
//      case Some(init) => Some(init).asInstanceOf[Option[Seq[member.TToken]]]
//      case None => calc.get(member).asInstanceOf[Option[Seq[member.TToken]]]
//    }
//    def getRetVal(cb : ConditionalBlock.WithRetVal[_]) : DFAny = {
//      designDB.ownerMemberTable(cb).last match {
//        case n : DFNet.Assignment if n.toRef.get == cb.retVarRef.get => n.fromRef.get
//        case m => throw new IllegalArgumentException(s"Unexpected last member in conditional block. Expected assignment to the return variable, but got $m")
//      }
//    }
//    remaining match {
//      case m :: mList => if (getInit(m).isDefined) calcInit(mList, calc, requestedCalc) else m match {
//        case c : DFAny.Const[_] => calcInit(mList, calc + (m -> Seq(c.token)), requestedCalc)
//        case f : DFAny.Func2[_,_,_,_] =>
//          val leftArg = f.leftArgRef.get
//          val rightArg = f.rightArgRef.get
//          (getInit(leftArg), getInit(rightArg)) match {
//            case (Some(leftInit), Some(rightInit)) =>
//              calcInit(mList, calc + (m -> f.initFunc(leftInit, rightInit)), requestedCalc)
//            case _ if requestedCalc.contains(m) =>
//              calcInit(mList, calc + (m -> Seq()), requestedCalc - m)
//            case _ =>
//              calcInit(leftArg :: rightArg :: remaining, calc, requestedCalc + m) //first need to calculate args
//          }
//        case a : DFAny.Alias[_,_,_] => designDB.getConnectionTo(a) match {
//          //connection overrides the calculated alias init
//          case Some(s) => getInit(s) match {
//            case Some(init) => calcInit(mList, calc + (m -> init), requestedCalc)
//            case None if requestedCalc.contains(m) => calcInit(mList, calc + (m -> Seq()), requestedCalc - m)
//            case None => calcInit(s :: remaining, calc, requestedCalc + m)
//          }
//          //no connection => use the calculated aliased init
//          case None =>
//            val relVal = a.relValRef.get
//            getInit(relVal) match {
//              case Some(relInit) => calcInit(mList, calc + (m -> a.initFunc(relInit)), requestedCalc)
//              case None if requestedCalc.contains(m) => calcInit(mList, calc + (m -> Seq()), requestedCalc - m)
//              case None => calcInit(relVal :: remaining, calc, requestedCalc + m)
//            }
//        }
//        case rv@DFAny.NewVar(_,DFAny.Modifier.MatchRetVar, _, _) =>
//          calcInit(mList, calc + (m -> Seq()), requestedCalc)
//        case rv@DFAny.NewVar(_,DFAny.Modifier.IfRetVar, _, _) =>
//          //            val members = designDB.ownerMemberTable(rv.getOwner)
//          //            val cbs = members.collect{case m : ConditionalBlock.WithRetVal[_] if m.retVarRef.get == rv => m}
//          //            val ifConds : List[Either[(DFBool, DFAny), DFAny]] = cbs.collect {
//          //              case b : ConditionalBlock.IfBlock => Left(b.condRef.get, getRetVal(b))
//          //              case b : ConditionalBlock.ElseIfBlock => Left(b.condRef.get, getRetVal(b))
//          //              case b : ConditionalBlock.ElseBlock => Right(getRetVal(b))
//          //            }
//          //            DFBool.Token.select()
//          //            val ifInits : List[Either[(Option[Seq[DFBool.Token]], Option[Seq[DFAny.Token]]), Option[Seq[DFAny.Token]]]] = ifConds.map {
//          //              case Left((cond, retVal)) => Left(getInit(cond), getInit(retVal))
//          //              case Right(retVal) => Right(getInit(retVal))
//          //            }
//          calcInit(mList, calc + (m -> Seq()), requestedCalc)
//        case v : DFAny.Value[_,_] => v.modifier match { //Handles NewVar, Port.In, Port.Out
//          //external init has priority over connection init
//          case i : DFAny.Modifier.Initialized[_] => calcInit(mList, calc + (m -> i.externalInit), requestedCalc)
//          case _ => designDB.getConnectionTo(v) match {
//            //uses connection init
//            case Some(s) => getInit(s) match {
//              case Some(init) => calcInit(mList, calc + (m -> init), requestedCalc)
//              case None if requestedCalc.contains(m) => calcInit(mList, calc + (m -> Seq()), requestedCalc - m)
//              case None => calcInit(s :: remaining, calc, requestedCalc + m)
//            }
//            //no connection and no external init, so use an empty sequence
//            case None =>
//              //TODO: add connection via alias init fetch support here
//              calcInit(mList, calc + (m -> Seq()), requestedCalc)
//          }
//        }
//      }
//      case Nil => calc
//    }
//  }
//
//  def calcInit : DFDesign.DB = {
//    //we request init calculation for all members that can have initialization and currently do not have
//    //a calculated init tag (the tag is empty).
//    val calcMembers = designDB.members.collect{case v : DFAny if v.tags.init.isEmpty => v}
//    val initMap = calcInit(calcMembers, Map(), Set())
//    designDB.patch(initMap.toList.map{
//      case (v, init) => v -> Patch.Replace(v.setTags(v.tags.setInit(init.asInstanceOf[Seq[v.TToken]])), Patch.Replace.Config.FullReplacement)
//    })
//  }
//}
//
