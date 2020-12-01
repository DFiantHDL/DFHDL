package DFiant
package compiler

import DFDesign.DB.Patch
import scala.annotation.tailrec

final class InitCalc[D <: DFDesign](c : IRCompilation[D]) {
  private val designDB = c.db
  import designDB.__getset
  object CalcDep {
    def unapply(member : DFAny.Member) : Option[(List[DFAny.Member], List[Seq[DFAny.Token]] => Seq[DFAny.Token])] = member match {
      case c : DFAny.Const => Some(Nil, _ => Seq(c.token))
      case f : DFAny.Func2 => Some(List(f.leftArgRef.get, f.rightArgRef.get), args => f.initFunc(args.head, args(1)))
      case asel : DFAny.Alias.ApplySel => Some(Nil, _ => Seq()) //TODO: add support for init
      case a : DFAny.Alias if designDB.getConnectionTo(a).isEmpty => Some(List(a.relValRef.get), args => a.initFunc(args.head))
      case f : DFAny.Fork => Some(List(f.relValRef.get), args => args.head)
      case rv@DFAny.Dcl(_,DFAny.Modifier.MatchRetVar, _, _, _) => Some(Nil, _ => Seq()) //TODO: add support for init
      case rv@DFAny.Dcl(_,DFAny.Modifier.IfRetVar, _, _, _) => Some(Nil, _ => Seq()) //TODO: add support for init
      case DFAny.Dcl(_,_,Some(init),_,_) => Some(Nil, _ => init) //external init
      case _ => None
    }
  }
  @tailrec private def initCalc(
    remaining : List[DFAny.Member], calc : Map[DFAny.Member, Seq[DFAny.Token]], requestedCalc : Set[DFAny.Member]
  ) : Map[DFAny.Member, Seq[DFAny.Token]] = {
    def getInit(member : DFAny.Member) : Option[Seq[DFAny.Token]] = member.getInit match {
      case Some(init) => Some(init)
      case None => calc.get(member)
    }
//    def getRetVal(cb : ConditionalBlock.WithRetVal[_]) : DFAny = {
//      designDB.blockMemberTable(cb).last match {
//        case n : DFNet.Assignment if n.toRef.get == cb.retVarRef.get => n.fromRef.get
//        case m => throw new IllegalArgumentException(s"Unexpected last member in conditional block. Expected assignment to the return variable, but got $m")
//      }
//    }
    remaining match {
      case m :: mList => if (getInit(m).isDefined) initCalc(mList, calc, requestedCalc) else m match {
        case CalcDep(deps, func) =>
          val initOptions = deps.flatMap(d => getInit(d))
          if (initOptions.length == deps.length) initCalc(mList, calc + (m -> func(initOptions)), requestedCalc)
          else initCalc(deps ++ remaining, calc, requestedCalc + m)
        case v : DFAny.Member => designDB.getConnectionTo(v) match {
          //uses connection init
          case Some(n) =>
            val s = n.fromRef.get
            getInit(s) match {
              case Some(init) => initCalc(mList, calc + (m -> init), requestedCalc)
              case None if requestedCalc.contains(m) =>
                //Connection loop returns an empty initialization
                initCalc(mList, calc + (m -> Seq()), requestedCalc - m)
              case None => initCalc(s :: remaining, calc, requestedCalc + m)
            }
          //no connection and no external init, so use an empty sequence
          case None =>
            //TODO: add connection via alias init fetch support here
            initCalc(mList, calc + (m -> Seq()), requestedCalc)
        }
      }
      case Nil => calc
    }
  }

  def initCalc : IRCompilation[D] = {
    //we request init calculation for all members that can have initialization and currently do not have
    //a calculated init tag (the tag is empty).
    val calcMembers = designDB.members.collect{case v : DFAny.Member if v.getInit.isEmpty => v}
    val initMap = initCalc(calcMembers, Map(), Set())
    val patchList = initMap.toList.map{
      case (v, init) => v -> Patch.Replace(v.setInit(init), Patch.Replace.Config.FullReplacement)
    }
    c.newStage(designDB.patch(patchList))
  }
  def clearInitCalc : IRCompilation[D] = {
    val calcMembers = designDB.members.collect{case v : DFAny.Member if v.getInit.nonEmpty => v}
    val patchList = calcMembers.map{m =>
      m -> Patch.Replace(m.clearInit, Patch.Replace.Config.FullReplacement)
    }
    c.newStage(designDB.patch(patchList))
  }
}