package DFiant
package compiler

import DFDesign.DB.Patch

final class ExplicitConversionsOps[D <: DFDesign, S <: shapeless.HList](c : IRCompilation[D, S]) {
  private val designDB = c.db
  import designDB.__getset
  private def resizeUInt(dfVal : DFAny, updatedWidth : Int) : (DFAny, Patch) = dfVal match {
    case DFAny.Const(dfType, token : DFUInt.Token, ownerRef, tags) =>
      val updatedConst : DFAny = DFAny.Const(dfType, token.resize(updatedWidth), ownerRef, tags)
      dfVal -> Patch.Replace(updatedConst, Patch.Replace.Config.FullReplacement)
    case _ =>
      dfVal -> Patch.Add(new MetaDesign() {
        dfVal.asInstanceOf[DFUInt[Int]].resize(updatedWidth)
      }, Patch.Add.Config.Via)
  }
  private def resizeSInt(dfVal : DFAny, updatedWidth : Int) : (DFAny, Patch) = dfVal match {
    case DFAny.Const(dfType, token : DFSInt.Token, ownerRef, tags) =>
      val updatedConst : DFAny = DFAny.Const(dfType, token.resize(updatedWidth), ownerRef, tags)
      dfVal -> Patch.Replace(updatedConst, Patch.Replace.Config.FullReplacement)
    case _ =>
      dfVal -> Patch.Add(new MetaDesign() {
        dfVal.asInstanceOf[DFSInt[Int]].resize(updatedWidth)
      }, Patch.Add.Config.Via)
  }
  private def toggleLogical(dfVal : DFAny) : (DFAny, Patch) = dfVal match {
    case DFAny.Const(_, DFBool.Token(logical, value, bubble), ownerRef, tags) =>
      val updatedConst : DFAny = DFAny.Const(DFBool.Type(!logical), DFBool.Token(!logical, value, bubble), ownerRef, tags)
      dfVal -> Patch.Replace(updatedConst, Patch.Replace.Config.FullReplacement)
    case DFBit() =>
      dfVal -> Patch.Add(new MetaDesign() {
        dfVal.asInstanceOf[DFBit] === 1
      }, Patch.Add.Config.Via)
    case DFBool() =>
      dfVal -> Patch.Add(new MetaDesign() {
        dfVal.asInstanceOf[DFBool].as(DFBool.Type(false))
      }, Patch.Add.Config.Via)
  }

  def explicitConversions = {
    val patchList = designDB.members.flatMap {
      case net : DFNet =>
        val toVal = net.toRef.get
        val fromVal = net.fromRef.get
        (toVal, fromVal) match {
          case (DFUInt(toWidth), DFUInt(fromWidth)) if toWidth > fromWidth => Some(resizeUInt(fromVal, toWidth))
          case (DFSInt(toWidth), DFSInt(fromWidth)) if toWidth > fromWidth => Some(resizeSInt(fromVal, toWidth))
          case (DFBool(), DFBit()) => Some(toggleLogical(fromVal))
          case (DFBit(), DFBool()) => Some(toggleLogical(fromVal))
          case _ => None
        }
      case func : DFAny.Func2 =>
        val leftArg = func.leftArgRef.get
        val rightArg = func.rightArgRef.get
        (leftArg, rightArg) match {
          case (DFBit(), DFBool()) => Some(toggleLogical(leftArg))
          case (DFBool(), DFBit()) => Some(toggleLogical(rightArg))
          case _ => None
        }
//      case cb : ConditionalBlock.IfBlock =>
//        val cond = cb.condRef.get
//        cond match {
//          case DFBit() => Some(toggleLogical(cond))
//          case _ => None
//        }
//      case cb : ConditionalBlock.ElseIfBlock =>
//        val cond = cb.condRef.get
//        cond match {
//          case DFBit() => Some(toggleLogical(cond))
//          case _ => None
//        }
//      case DFSimMember.Assert(Some(condRef), _, _, _, _) =>
//        val cond = condRef.get
//        cond match {
//          case DFBit() => Some(toggleLogical(cond))
//          case _ => None
//        }
      case _ => None
    }
    c.newStage[ExplicitConversions](designDB.patch(patchList))
  }
}

trait ExplicitConversions extends Compilation.Stage