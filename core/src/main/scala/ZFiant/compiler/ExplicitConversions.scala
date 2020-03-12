package ZFiant
package compiler

import DFDesign.DB.Patch

final class ExplicitConversionsOps[D <: DFDesign, S <: shapeless.HList](c : Compilable[D, S]) {
  private val designDB = c.db
  import designDB.__getset
  private def resizeUInt(dfVal : DFAny, updatedWidth : Int) : (DFAny, Patch) = dfVal match {
    case DFAny.Const(dfType, token : DFUInt.Token[_], ownerRef, tags) =>
      val updatedConst : DFAny = DFAny.Const(dfType, token.resize(updatedWidth), ownerRef, tags)
      dfVal -> Patch.Replace(updatedConst, Patch.Replace.Config.FullReplacement)
    case _ =>
      dfVal -> Patch.Add(new MetaDesign() {
        dfVal.asInstanceOf[DFUInt[Int]].resize(updatedWidth)
      }, Patch.Add.Config.Via)
  }
  private def resizeSInt(dfVal : DFAny, updatedWidth : Int) : (DFAny, Patch) = dfVal match {
    case DFAny.Const(dfType, token : DFSInt.Token[_], ownerRef, tags) =>
      val updatedConst : DFAny = DFAny.Const(dfType, token.resize(updatedWidth), ownerRef, tags)
      dfVal -> Patch.Replace(updatedConst, Patch.Replace.Config.FullReplacement)
    case _ =>
      dfVal -> Patch.Add(new MetaDesign() {
        dfVal.asInstanceOf[DFSInt[Int]].resize(updatedWidth)
      }, Patch.Add.Config.Via)
  }
  private def as(dfVal : DFAny, updateDFType : DFAny.Type) : (DFAny, Patch) = {
    dfVal -> Patch.Add(new MetaDesign() {
      dfVal.asInstanceOf[DFAny.Of[DFAny.Type]].as(updateDFType)
    }, Patch.Add.Config.Via)
  }

  def explicitResizes = {
    val patchList = designDB.members.flatMap {
      case net : DFNet =>
        val toVal = net.toRef.get
        val fromVal = net.fromRef.get
        (toVal, fromVal) match {
          case (DFUInt(toWidth), DFUInt(fromWidth)) if toWidth > fromWidth => Some(resizeUInt(fromVal, fromWidth))
          case (DFSInt(toWidth), DFSInt(fromWidth)) if toWidth > fromWidth => Some(resizeSInt(fromVal, fromWidth))
          case (DFBool(), DFBit()) => Some(as(fromVal, toVal.dfType))
          case (DFBit(), DFBool()) => Some(as(fromVal, toVal.dfType))
          case _ => None
        }
      case func : DFAny.Func2 =>
        val leftArg = func.leftArgRef.get
        val rightArg = func.rightArgRef.get
        (leftArg, rightArg) match {
          case (DFBit(), DFBool()) => Some(as(leftArg, DFBool.Type(true)))
          case (DFBool(), DFBit()) => Some(as(rightArg, DFBool.Type(true)))
          case _ => None
        }
      case cb : ConditionalBlock.IfBlock =>
        val cond = cb.condRef.get
        cond match {
          case DFBit() => Some(as(cond, DFBool.Type(true)))
          case _ => None
        }
      case cb : ConditionalBlock.ElseIfBlock =>
        val cond = cb.condRef.get
        cond match {
          case DFBit() => Some(as(cond, DFBool.Type(true)))
          case _ => None
        }
      case _ => None
    }
    c.newStage[ExplicitConversions](designDB.patch(patchList), Seq())
  }
}

trait ExplicitConversions extends Compilable.Stage