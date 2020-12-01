package DFiant
package compiler

import DFDesign.DB.Patch

import scala.annotation.tailrec
import scala.reflect.{ClassTag, classTag}

final class FlattenNames[D <: DFDesign](c : IRCompilation[D]) {
  private val designDB = c.db
  import designDB.__getset
  @tailrec private def recursiveNameFlatten(member : DFMember, nameFunc : String => String) : String => String = member.getOwner match {
    case o : DFOwner.NameFlattenOwner =>
      o.nameFlatten match {
        case DFOwner.NameFlatten.IgnoreOwnerName => recursiveNameFlatten(o, nameFunc)
        case _ => recursiveNameFlatten(o, n => o.nameFlatten(nameFunc(n), o.name))
      }
    case _ => nameFunc
  }
  private def recursiveNameFlatten(member : DFMember) : String => String = recursiveNameFlatten(member, n => n)
  def flattenNames : IRCompilation[D] = {
    val (patchList, tagList) = designDB.members.foldRight((List.empty[(DFMember, Patch)], List.empty[((Any, ClassTag[_]), DFMember.CustomTag)])) {
      case (_ : DFDesign.Block.Top, ret) => ret
      case (o : DFOwner.NameFlattenOwner, ret) =>
        (o -> Patch.Replace(o.getOwnerBlock, Patch.Replace.Config.ChangeRefAndRemove) :: ret._1, ret._2)
      case (m, ret) if !m.isAnonymous => m.getOwner match {
        case _ : DFOwner.NameFlattenOwner =>
          val updatedNameFunc = recursiveNameFlatten(m)
          val updatedName = updatedNameFunc(m.name)
          val updatedTags = m match {
            case DFAny.Dcl(DFEnum.Type(enumType),DFAny.Modifier.NewVar,_,_,_) =>
              val tag = (enumType, classTag[EnumType.NameTag]) -> EnumType.NameTag(updatedNameFunc(enumType.name))
              tag :: ret._2
            case _ =>
              ret._2
          }
          val updatedMember = m.setName(updatedName)
          if (updatedMember == m) ret
          else (m -> Patch.Replace(updatedMember, Patch.Replace.Config.FullReplacement) :: ret._1, updatedTags)
        case _ => ret
      }
      case (_, ret) => ret
    }
    c.newStage(designDB.patch(patchList).setGlobalTags(tagList))
  }
}