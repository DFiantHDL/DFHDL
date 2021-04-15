package DFiant
package compiler

import DFDesign.DB.Patch
import DFiant.internals.IntExtras

import scala.annotation.tailrec
import scala.collection.mutable

final class FlattenTypes[D <: DFDesign](c : IRCompilation[D]) {
  object FlattenableType {
    def unapply(member : DFAny.Member) : Boolean = member match {
      case DFVector(_,cellNum) if cellNum <= 16 => true
      case DFStruct(_) => true
      case _ => false
    }
  }

  private def newVectorFieldName(parentFieldName : String, cellNum : Int, cellIdx : Int) : String =
    s"${parentFieldName}_${cellIdx.toPaddedString(cellNum)}"
  private def newStructFieldName(
    parentFieldName : String, fields : DFStruct.Fields, fieldName : String
  ) : String = fields match {
    case _: DFOpaque.Fields => parentFieldName //opaque has no suffix (no need for "actual")
    case _: DFTuple.Fields => s"${parentFieldName}${fieldName}" //tuple field names already include underscore
    case _ => s"${parentFieldName}_${fieldName}" //any other struct field
  }

  @tailrec private def getFlattenedFieldsDataRecur[T](
    vecDataMap : (T, Int) => T,
    structDataMap : (T, DFStruct.Field[_ <: DFAny.Type]) => T
  )(typeQueue : List[(DFAny.Type, String, T)], fieldList : List[(DFAny.Type, String, T)])(
    implicit getSet: MemberGetSet
  ) : List[(DFAny.Type, String, T)] = typeQueue match {
    case ::(head @ (parentFieldType, parentFieldName, parentFieldData), next) =>
      parentFieldType match {
        case DFVector.Type(cellType,cellNum) =>
          val updatedTypeQueue =
            (0 until cellNum).view.map(i =>
              (cellType,newVectorFieldName(parentFieldName, cellNum, i), vecDataMap(parentFieldData, i))
            ) ++ next
          getFlattenedFieldsDataRecur[T](vecDataMap, structDataMap)(updatedTypeQueue.toList, fieldList)
        case DFStruct.Type(fields) =>
          val updatedTypeQueue =
            fields.getFieldList.view.map {case f : DFStruct.Field[DFAny.Type @unchecked]=>
              (f.dfType, newStructFieldName(parentFieldName, fields, f.fieldName), structDataMap(parentFieldData, f))
            } ++ next
          getFlattenedFieldsDataRecur[T](vecDataMap, structDataMap)(updatedTypeQueue.toList, fieldList)
        case _ => getFlattenedFieldsDataRecur[T](vecDataMap, structDataMap)(next, head :: fieldList)
      }
    case Nil => fieldList
  }

  private def genField(member : DFAny.Member, memberCreation : => DFAny.Member, fieldName : String)(
    implicit getSet: MemberGetSet, fieldMemberMap : mutable.Map[(DFAny.Member, String), DFAny.Member]
  ) : DFAny.Member = {
    val fieldVal = if (member.isAnonymous) memberCreation.anonymize
    else memberCreation.setName(s"${member.name}$fieldName")
    fieldMemberMap += (member, fieldName) -> fieldVal
    fieldVal
  }
  private def getFlattenedFieldsData[T, M <: DFAny.Member](member : M)(
    vecDataMap : (T, Int) => T,
    structDataMap : (T, DFStruct.Field[_ <: DFAny.Type]) => T,
    memberDataMap : M => T,
    memberCreation : (DFAny.Type, T) => DFAny.Member
  )(implicit
    getSet: MemberGetSet, fieldMemberMap : mutable.Map[(DFAny.Member, String), DFAny.Member]
  ) : List[DFAny.Member] = {
    getFlattenedFieldsDataRecur[T](vecDataMap, structDataMap)(
      List((member.dfType, "", memberDataMap(member))), Nil
    ).view.reverse.map { case (fieldType, fieldName, fieldData) =>
      genField(member, memberCreation(fieldType, fieldData), fieldName)
    }.toList
  }

  @tailrec private def getFlattenedFieldsRecur(
    typeQueue : List[(DFAny.Type, String)], fieldList : List[(DFAny.Type, String)]
  ) : List[(DFAny.Type, String)] = typeQueue match {
    case ::(head @ (parentFieldType, parentFieldName), next) =>
      parentFieldType match {
        case DFVector.Type(cellType,cellNum) =>
          val updatedTypeQueue =
            (0 until cellNum).view.map(i =>
              (cellType,newVectorFieldName(parentFieldName, cellNum, i))
            ) ++ next
          getFlattenedFieldsRecur(updatedTypeQueue.toList, fieldList)
        case DFStruct.Type(fields) =>
          val updatedTypeQueue =
            fields.getFieldList.view.map {case f : DFStruct.Field[DFAny.Type @unchecked]=>
              (f.dfType, newStructFieldName(parentFieldName, fields, f.fieldName))
            } ++ next
          getFlattenedFieldsRecur(updatedTypeQueue.toList, fieldList)
        case _ => getFlattenedFieldsRecur(next, head :: fieldList)
      }
    case Nil => fieldList
  }
  def getFlattenedFields(dfType : DFAny.Type) : List[(DFAny.Type, String)] =
    getFlattenedFieldsRecur(List((dfType, "")), Nil).reverse

  @tailrec private def getSelectedFieldRecur(member : DFAny.Member, childFieldName : String = "")(implicit
    getSet: MemberGetSet, fieldMemberMap : mutable.Map[(DFAny.Member, String), DFAny.Member]
  ) : DFAny.Member = member match {
    case DFAny.ApplySel.Unref(_,_, relVal @ DFVector(_, cellNum), DFAny.ApplySel.ConstIdx(idx),_,_) =>
      getSelectedFieldRecur(relVal, newVectorFieldName("", cellNum, idx) concat childFieldName)
    case DFStruct.Selector.Unref(_,_, struct @ DFStruct(fields), fieldName,_,_) =>
      getSelectedFieldRecur(struct, newStructFieldName("", fields, fieldName) concat childFieldName)
    case _ => fieldMemberMap(member, childFieldName)
  }

  def flattenTypes : IRCompilation[D] = {
    val designDB = c.db
    val dbgs = designDB.__getset
    import designDB.__getset
    //Collecting all vector types that have non-constant indexing
    val dontFlattenTypes : Set[DFAny.Type] = designDB.members.view.flatMap {
      case DFAny.ApplySel.Unref(_, _, relVal @ DFVector(_, _), idx, _, _) => idx match {
        case _ : DFAny.Const => None
        case _ => Some(relVal.dfType)
      }
      case _ => None
    }.toSet
    implicit val fieldMemberMap = mutable.Map.empty[(DFAny.Member, String), DFAny.Member]
    val patchList = designDB.members.flatMap {
//      //special casing cast to opaque
      case m @ DFAny.Alias.AsIs.Unref(DFOpaque.Type(dfType),_,relVal,_,_) if dfType == relVal.dfType =>
        fieldMemberMap += (m, "") -> relVal
        List(m -> Patch.Replace(relVal, Patch.Replace.Config.ChangeRefAndRemove))
//
//      case alias : DFAny.Alias if alias.relValRef.get.isInstanceOf[DFStruct.Selector] => Nil
      case m : DFAny.Member if dontFlattenTypes.contains(m.dfType) => Nil
      case m @(_ : DFAny.ApplySel | _ : DFStruct.Selector) => m match {
        case FlattenableType() => List(m -> Patch.Remove)
        case applySel : DFAny.ApplySel if dontFlattenTypes.contains(applySel.relValRef.get.dfType) => Nil
        case anymember : DFAny.Member =>
          List(anymember -> Patch.Replace(getSelectedFieldRecur(anymember), Patch.Replace.Config.ChangeRefAndRemove))
      }
      case m @ FlattenableType() =>
        val dsn = new MetaDesign() {
          val addedFields = m match {
            case const : DFAny.Const =>
              val vecDataMap : (DFAny.Token, Int) => DFAny.Token =
                (token, idx) => token.asInstanceOf[DFVector.Token].value(idx)
              val structDataMap : (DFAny.Token, DFStruct.Field[_ <: DFAny.Type]) => DFAny.Token =
                (token, field) => token.asInstanceOf[DFStruct.Token].value(field)
              val memberDataMap : DFAny.Const => DFAny.Token = _.token
              val memberCreation : (DFAny.Type, DFAny.Token) => DFAny.Member =
                (dfType, token) => DFAny.Const.forced(token).member
              getFlattenedFieldsData(const)(vecDataMap, structDataMap, memberDataMap, memberCreation)
            case dcl : DFAny.Dcl =>
              val vecDataMap : (Option[Seq[DFAny.Token]], Int) => Option[Seq[DFAny.Token]] =
                (externalInit, idx) => externalInit.asInstanceOf[Option[Seq[DFVector.Token]]].map(_.map(_.value(idx)))
              val structDataMap : (Option[Seq[DFAny.Token]], DFStruct.Field[_ <: DFAny.Type]) => Option[Seq[DFAny.Token]] =
                (externalInit, field) => externalInit.asInstanceOf[Option[Seq[DFStruct.Token]]].map(_.map(_.value(field)))
              val memberDataMap : DFAny.Dcl => Option[Seq[DFAny.Token]] = _.externalInit
              val memberCreation : (DFAny.Type, Option[Seq[DFAny.Token]]) => DFAny.Member =
                (dfType, externalInit) => DFAny.Dcl(dfType, dcl.modifier, externalInit).member
              getFlattenedFieldsData(dcl)(vecDataMap, structDataMap, memberDataMap, memberCreation)
            case prev : DFAny.Alias.Prev =>
              val fieldList = getFlattenedFields(prev.dfType)
              val relVal = prev.relValRef.get(dbgs)
              fieldList.map { case (_, fieldName) =>
                val fieldRelVal = fieldMemberMap(relVal, fieldName)
                genField(prev, DFAny.Alias.Prev(fieldRelVal, prev.step, prev.kind).member, fieldName)
              }
            case asis : DFAny.Alias.AsIs => ???
          }
        }
        List(m -> Patch.Add(dsn, Patch.Add.Config.After), m -> Patch.Remove)
//
//      case bitsSel @ DFAny.Alias.BitsWL.Unref(_, _, relVal @ DFStruct(fields), relWidth, relBitLow, _, _) =>
//        val fieldList = fields.getFieldList
//        val dsn = new MetaDesign() {
//          private def addConcat = {
//            val concatBits = fieldList.map {
//              case f : DFStruct.Field[DFAny.Type @unchecked] =>
//                fieldMemberMap((relVal, f.fieldName)).asVal.bits.anonymize.asValOf[DFBits.Type[Int]]
//            }.reduce[DFBits[Int]](_ ++ _)
//            if (relWidth == relVal.width) concatBits else concatBits.anonymize.bitsWL(relWidth, relBitLow)
//          }
//          if (bitsSel.isAnonymous) addConcat.anonymize else addConcat.setName(bitsSel.name)
//        }
//        List(bitsSel -> Patch.Add(dsn, Patch.Add.Config.ReplaceWithLast()))
      case net @ DFNet.Unref(toVal @ FlattenableType(), op, fromVal, _, _) =>
        val fieldList = getFlattenedFields(toVal.dfType)
        val dsn = new MetaDesign() {
          fieldList.foreach { case (_, fieldName) =>
            DFNet(fieldMemberMap((toVal, fieldName)), op, fieldMemberMap((fromVal, fieldName)))
          }
        }
        List(net -> Patch.Add(dsn, Patch.Add.Config.After), net -> Patch.Remove)
      case _ => Nil
    }

    c.newStage(designDB.patch(patchList))
  }
}