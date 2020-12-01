package DFiant
package compiler

import DFDesign.DB.Patch
import DFDesign.Frontend._
import DFiant.DFAny.Func2.Op
final class ExplicitConversions[D <: DFDesign](c : IRCompilation[D]) {
  private val designDB = c.db
  private def resizeUInt(dfVal : DFAny.Member, updatedWidth : Int)(implicit ctx : DFBlock.Context) : DFAny.Member = dfVal match {
    case DFAny.Const(dfType, token : DFUInt.Token, ownerRef, tags) =>
      DFAny.Const(dfType, token.resize(updatedWidth), ownerRef, tags)
    case _ =>
      dfVal.asValOf[DFUInt.Type[Int]].resize(updatedWidth).anonymize.member
  }
  private def resizeSInt(dfVal : DFAny.Member, updatedWidth : Int)(implicit ctx : DFBlock.Context) : DFAny.Member = dfVal match {
    case DFAny.Const(dfType, token : DFSInt.Token, ownerRef, tags) =>
      DFAny.Const(dfType, token.resize(updatedWidth), ownerRef, tags)
    case _ =>
      dfVal.asValOf[DFSInt.Type[Int]].resize(updatedWidth).anonymize.member
  }
  private def toggleLogical(dfVal : DFAny.Member)(implicit ctx : DFBlock.Context) : DFAny.Member = dfVal match {
    case DFAny.Const(_, DFBool.Token(logical, value), ownerRef, tags) =>
      DFAny.Const(DFBool.Type(!logical), DFBool.Token(!logical, value), ownerRef, tags)
    case DFBit() =>
      (dfVal.asValOf[DFBool.Type] === 1).anonymize.member
    case DFBool() =>
      dfVal.asValOf[DFBool.Type].as(DFBit()).anonymize.member
  }

  import designDB.__getset
  //with-carry arithmetics function on uint/sint
  def carryMathConversion : IRCompilation[D] = {
    val patchList = designDB.members.flatMap {
      case func @ DFAny.Func2.Unref(dfType, leftArg, op : DFAny.Func2.Op.Carry, rightArg,_,_) =>
        val dsn = new MetaDesign() {
          private val opNC = op match {
            case Op.+^ => Op.+
            case Op.-^ => Op.-
            case Op.*^ => Op.*
          }
          private val tokenFuncNC : (_ <: DFAny.Token, _ <: DFAny.Token) => DFAny.Token = (op, dfType) match {
            case (Op.+^, DFUInt.Type(_)) => (l : DFUInt.Token, r : DFUInt.Token) => l + r
            case (Op.-^, DFUInt.Type(_)) => (l : DFUInt.Token, r : DFUInt.Token) => l - r
            case (Op.*^, DFUInt.Type(_)) => (l : DFUInt.Token, r : DFUInt.Token) => l * r
            case (Op.+^, DFSInt.Type(_)) => (l : DFSInt.Token, r : DFSInt.Token) => l + r
            case (Op.-^, DFSInt.Type(_)) => (l : DFSInt.Token, r : DFSInt.Token) => l - r
            case (Op.*^, DFSInt.Type(_)) => (l : DFSInt.Token, r : DFSInt.Token) => l * r
            case _ => ???
          }
          private val (left, right) = dfType match {
            case DFUInt.Type(targetWidth) =>
              if (leftArg.width >= rightArg.width)
                (leftArg.asValOf[DFUInt.Type[Int]].resize(targetWidth).member.anonymize, rightArg)
              else (leftArg, rightArg.asValOf[DFUInt.Type[Int]].resize(targetWidth).member.anonymize)
            case DFSInt.Type(targetWidth) =>
              if (leftArg.width >= rightArg.width)
                (leftArg.asValOf[DFSInt.Type[Int]].resize(targetWidth).member.anonymize, rightArg)
              else (leftArg, rightArg.asValOf[DFSInt.Type[Int]].resize(targetWidth).member.anonymize)
          }
          DFAny.Func2.forced(dfType, left, opNC, right)(tokenFuncNC) setTags(_ => func.tags)
        }
        Some(func -> Patch.Add(dsn, Patch.Add.Config.ReplaceWithLast(Patch.Replace.Config.FullReplacement)))
      case _ => None
    }
    c.newStage(designDB.patch(patchList))
  }

  def explicitConversions : IRCompilation[D] = {
    val patchList = designDB.members.flatMap {
      //explicit widening when assigning/connecting to a wider variable or converting bit/bool
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
              case (DFUInt(toWidth), DFUInt(fromWidth)) if toWidth > fromWidth =>
                fromVal.asValOf[DFUInt.Type[Int]].resize(toWidth).member.anonymize
              case (DFSInt(toWidth), DFSInt(fromWidth)) if toWidth > fromWidth =>
                fromVal.asValOf[DFSInt.Type[Int]].resize(toWidth).member.anonymize
              case (DFBool(), DFBit()) => toggleLogical(fromVal)
              case (DFBit(), DFBool()) => toggleLogical(fromVal)
            }
            toVal.assign(updatedFromVal)
          }
          Some(net -> Patch.Add(dsn, Patch.Add.Config.ReplaceWithLast(Patch.Replace.Config.FullReplacement)))
        } else None
      //function between a bit and a boolean
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
      //conditional on bit
      case cb @ DFConditional.IfElseBlock(Some(condRef),prevBlockRefOption,_,_) =>
        val cond = condRef.get
        val prevIfOption = prevBlockRefOption.map(r => r.get)
        cond match {
          case DFBit() =>
            val dsn = new MetaDesign() {
              DFConditional.IfElseBlock(Some(toggleLogical(cond)), prevIfOption)
            }
            Some(cb -> Patch.Add(dsn, Patch.Add.Config.ReplaceWithLast(Patch.Replace.Config.FullReplacement)))
          case _ => None
        }
      //assert on bit
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
