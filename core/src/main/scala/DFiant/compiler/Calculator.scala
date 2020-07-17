package DFiant
package compiler

import DFDesign.DB.Patch
import scala.annotation.tailrec

final class CalculatorOps[D <: DFDesign, S <: shapeless.HList](c : IRCompilation[D, S]) {
  private val designDB = c.db
  import designDB.__getset
  @tailrec private def calcInitRec(remaining : List[DFAny], calc : Map[DFAny, Seq[DFAny.Token]], requestedCalc : Set[DFAny]) : Map[DFAny, Seq[DFAny.Token]] = {
    def getInit(member : DFAny) : Option[Seq[DFAny.Token]] = member.getInit match {
      case Some(init) => Some(init)
      case None => calc.get(member)
    }
    def getRetVal(cb : ConditionalBlock.WithRetVal[_]) : DFAny = {
      designDB.blockMemberTable(cb).last match {
        case n : DFNet.Assignment if n.toRef.get == cb.retVarRef.get => n.fromRef.get
        case m => throw new IllegalArgumentException(s"Unexpected last member in conditional block. Expected assignment to the return variable, but got $m")
      }
    }
    object CalcDep {
      def unapply(member : DFAny) : Option[(List[DFAny], List[Seq[DFAny.Token]] => Seq[DFAny.Token])] = member match {
        case c : DFAny.Const => Some(Nil, _ => Seq(c.token))
        case f : DFAny.Func2 => Some(List(f.leftArgRef.get, f.rightArgRef.get), args => f.initFunc(args.head, args(1)))
        case a : DFAny.Alias[_,_,_] if designDB.getConnectionTo(a).isEmpty => Some(List(a.relValRef.get), args => a.initFunc(args.head))
        case f : DFAny.Fork => Some(List(f.relValRef.get), args => args.head)
        case rv@DFAny.Dcl(_,DFAny.Modifier.MatchRetVar, _, _, _) => Some(Nil, _ => Seq()) //TODO: add support for init
        case rv@DFAny.Dcl(_,DFAny.Modifier.IfRetVar, _, _, _) => Some(Nil, _ => Seq()) //TODO: add support for init
        case DFAny.Dcl(_,_,Some(init),_,_) => Some(Nil, _ => init) //external init
        case _ => None
      }
    }
    remaining match {
      case m :: mList => if (getInit(m).isDefined) calcInitRec(mList, calc, requestedCalc) else m match {
        case CalcDep(deps, func) =>
          val initOptions = deps.flatMap(d => getInit(d))
          if (initOptions.length == deps.length) calcInitRec(mList, calc + (m -> func(initOptions)), requestedCalc)
          else calcInitRec(deps ++ remaining, calc, requestedCalc + m)
        case v : DFAny.Value[_,_] => designDB.getConnectionTo(v) match {
          //uses connection init
          case Some(s) => getInit(s) match {
            case Some(init) => calcInitRec(mList, calc + (m -> init), requestedCalc)
            case None if requestedCalc.contains(m) =>
              //Connection loop returns an empty initialization
              calcInitRec(mList, calc + (m -> Seq()), requestedCalc - m)
            case None => calcInitRec(s :: remaining, calc, requestedCalc + m)
          }
          //no connection and no external init, so use an empty sequence
          case None =>
            //TODO: add connection via alias init fetch support here
            calcInitRec(mList, calc + (m -> Seq()), requestedCalc)
        }
      }
      case Nil => calc
    }
  }

  def calcInit = {
    //we request init calculation for all members that can have initialization and currently do not have
    //a calculated init tag (the tag is empty).
    val calcMembers = designDB.members.collect{case v : DFAny if v.getInit.isEmpty => v}
    val initMap = calcInitRec(calcMembers, Map(), Set())
    val patchList = initMap.toList.map{
      case (v, init) => v -> Patch.Replace(v.setInit(init), Patch.Replace.Config.FullReplacement)
    }
    c.newStage[Calculator](designDB.patch(patchList))
  }
}

trait Calculator extends Compilation.Stage