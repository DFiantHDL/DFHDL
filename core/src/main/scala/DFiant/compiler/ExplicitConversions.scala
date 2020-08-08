package DFiant
package compiler

import DFDesign.DB.Patch
import DFDesign.Implicits._
final class ExplicitConversions[D <: DFDesign](c : IRCompilation[D]) {
  private val designDB = c.db
  private def resizeUInt(dfVal : DFAny, updatedWidth : Int)(implicit ctx : DFBlock.Context) : DFAny = dfVal match {
    case DFAny.Const(dfType, token : DFUInt.Token, ownerRef, tags) =>
      DFAny.Const(dfType, token.resize(updatedWidth), ownerRef, tags)
    case _ =>
      dfVal.asInstanceOf[DFUInt[Int]].resize(updatedWidth).anonymize
  }
  private def resizeSInt(dfVal : DFAny, updatedWidth : Int)(implicit ctx : DFBlock.Context) : DFAny = dfVal match {
    case DFAny.Const(dfType, token : DFSInt.Token, ownerRef, tags) =>
      DFAny.Const(dfType, token.resize(updatedWidth), ownerRef, tags)
    case _ =>
      dfVal.asInstanceOf[DFSInt[Int]].resize(updatedWidth).anonymize
  }
  private def toggleLogical(dfVal : DFAny)(implicit ctx : DFBlock.Context) : DFBool = dfVal match {
    case DFAny.Const(_, DFBool.Token(logical, value), ownerRef, tags) =>
      DFAny.Const(DFBool.Type(!logical), DFBool.Token(!logical, value), ownerRef, tags).asInstanceOf[DFBool]
    case DFBit() =>
      (dfVal.asInstanceOf[DFBit] === 1).anonymize
    case DFBool() =>
      dfVal.asInstanceOf[DFBool].as(DFBool.Type(false)).anonymize
  }

  import designDB.__getset
  def explicitConversions : IRCompilation[D] = {
    val patchList = designDB.members.flatMap {
      case net : DFNet =>
        val toVal = net.toRef.get
        val fromVal = net.fromRef.get
        val conv = (toVal, fromVal) match {
          case (DFUInt(toWidth), DFUInt(fromWidth)) if toWidth > fromWidth => true
          case (DFSInt(toWidth), DFSInt(fromWidth)) if toWidth > fromWidth => true
          case (DFBool(), DFBit()) => true
          case (DFBit(), DFBool()) => true
          case _ => false
        }
        if (conv) {
          val dsn = new MetaDesign() {
            val updatedFromVal = (toVal, fromVal) match {
              case (DFUInt(toWidth), DFUInt(fromWidth)) if toWidth > fromWidth => resizeUInt(fromVal, toWidth)
              case (DFSInt(toWidth), DFSInt(fromWidth)) if toWidth > fromWidth => resizeSInt(fromVal, toWidth)
              case (DFBool(), DFBit()) => toggleLogical(fromVal)
              case (DFBit(), DFBool()) => toggleLogical(fromVal)
            }
            toVal.assign(updatedFromVal)
          }
          Some(net -> Patch.Add(dsn, Patch.Add.Config.ReplaceWithLast(Patch.Replace.Config.FullReplacement)))
        } else None
      case func : DFAny.Func2 =>
        val leftArg = func.leftArgRef.get
        val rightArg = func.rightArgRef.get
        val conv = (leftArg, rightArg) match {
          case (DFBit(), DFBool()) => true
          case (DFBool(), DFBit()) => true
          case _ => false
        }
        if (conv) {
          val dsn = new MetaDesign() {
            (leftArg, rightArg) match {
              case (DFBit(), DFBool()) =>
                DFAny.Func2.forced(DFBool.Type(true), toggleLogical(leftArg), func.op, rightArg)(func.tokenFunc).anonymize
              case (DFBool(), DFBit()) =>
                DFAny.Func2.forced(DFBool.Type(true), leftArg, func.op, toggleLogical(rightArg))(func.tokenFunc).anonymize
              case _ => None
            }
          }
          Some(func -> Patch.Add(dsn, Patch.Add.Config.ReplaceWithLast(Patch.Replace.Config.FullReplacement)))
        } else None
      case cb @ ConditionalBlock.IfElseBlock(Some(condRef),prevBlockRefOption,_,_) =>
        val cond = condRef.get
        val prevIfOption = prevBlockRefOption.map(r => r.get)
        cond match {
          case DFBit() =>
            val dsn = new MetaDesign() {
              ConditionalBlock.IfElseBlock(Some(toggleLogical(cond)), prevIfOption)
            }
            Some(cb -> Patch.Add(dsn, Patch.Add.Config.ReplaceWithLast(Patch.Replace.Config.FullReplacement)))
          case _ => None
        }
      case asrt @ sim.DFSimMember.Assert.Unref(Some(cond @ DFBit()), msg, severity, _, _) =>
        val dsn = new MetaDesign() {
          sim.DFSimMember.Assert(Some(toggleLogical(cond).asInstanceOf[DFBool]), msg, severity)
        }
        Some(asrt -> Patch.Add(dsn, Patch.Add.Config.ReplaceWithLast(Patch.Replace.Config.FullReplacement)))
      case _ => None
    }
    c.newStage(designDB.patch(patchList))
  }
}
