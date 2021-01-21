package DFiant
package compiler

import DFDesign.DB.Patch

import scala.collection.mutable

final class FlattenStruct[D <: DFDesign](c : IRCompilation[D]) {
  protected def privFlattenStruct : (Boolean, IRCompilation[D]) = {
    val designDB = c.db
    val dbgs = designDB.__getset
    import designDB.__getset
    val fieldMemberMap = mutable.Map.empty[(DFAny.Member, String), DFAny.Member]
    val patchList = designDB.members.flatMap {
      //special casing cast to opaque
      case m @ DFAny.Alias.AsIs.Unref(DFOpaque.Type(dfType),_,relVal,_,_) if dfType == relVal.dfType =>
        fieldMemberMap += (m, "actual") -> relVal
        List(m -> Patch.Replace(relVal, Patch.Replace.Config.ChangeRefAndRemove))

      case alias : DFAny.Alias if alias.relValRef.get.isInstanceOf[DFStruct.Selector] => Nil
      case selector @ DFStruct.Selector(_,_,structRef,fieldName,_,_) =>
        val structMember = structRef.get
        fieldMemberMap.get((structMember, fieldName)) match {
          case Some(value) =>
            Some(selector -> Patch.Replace(value, Patch.Replace.Config.ChangeRefAndRemove))
          case None => Nil
        }
      //Vector of struct is changed into vector of bits
      case m @ DFVector(cellType @ DFStruct.Type(_), cellNum) =>
        val updatedDFType = DFVector.Type(DFBits(cellType.width), cellNum)
        val replacement = m match {
          case const : DFAny.Const =>
            val tokenBits = const.token match {
              case DFVector.Token(cellType, value) => DFVector.Token(cellType, value.map(_.bits))
            }
            const.copy(token = tokenBits)
          case dcl : DFAny.Dcl =>
            val initOfBits = dcl.externalInit.map(_.map {
              case DFVector.Token(cellType, value) => DFVector.Token(cellType, value.map(_.bits))
            })
            dcl.copy(dfType = updatedDFType, externalInit = initOfBits)
          case alias : DFAny.Alias.AsIs => alias.copy(dfType = updatedDFType)
          case alias : DFAny.Alias.Prev => alias.copy(dfType = updatedDFType)
          case applySel : DFAny.ApplySel => applySel.copy(dfType = updatedDFType)
          case x =>
            println(x)
            ???
        }
        Some(m -> Patch.Replace(replacement, Patch.Replace.Config.FullReplacement))
      case m @ DFStruct(fields) =>
        val fieldList = fields.getFieldList
        var relBitHigh = m.width-1
        val dsn = new MetaDesign() {
          private lazy val structBits = m match {
            case applySel : DFAny.ApplySel => plantMember(applySel.copy(dfType = DFBits(m.width))).asVal
          }
          fieldList.foreach { case f : DFStruct.Field[DFAny.Type @unchecked] =>
            val relWidth = f.dfType.width.getValue
            val relBitLow = relBitHigh - relWidth + 1
            relBitHigh = relBitLow - 1
            def addField =  m match {
              case const : DFAny.Const =>
                val tokenValues = const.token match {
                  case DFStruct.Token(_, tokenValues) => tokenValues
                }
                DFAny.Const.forced(tokenValues(f))
              case dcl : DFAny.Dcl =>
                val fieldExternalInit : Option[Seq[DFAny.Token]] = dcl.externalInit match {
                  case Some(tokenSeq) =>
                    val seq = tokenSeq.asInstanceOf[Seq[DFStruct.Token]].flatMap(t => t.value.get(f))
                    if (seq.nonEmpty) Some(seq)
                    else None
                  case None => None
                }
                DFAny.Dcl(f.dfType, dcl.modifier, fieldExternalInit)
              case prev : DFAny.Alias.Prev =>
                val fieldRelVal = fieldMemberMap((prev.relValRef.get(dbgs), f.fieldName))
                DFAny.Alias.Prev(fieldRelVal, prev.step, prev.kind)
              case asis : DFAny.Alias.AsIs =>
                val structBits = asis.relValRef.get(dbgs).asVal
                structBits.bitsWL(relWidth, relBitLow).anonymize.as(f.dfType)
              case applySel : DFAny.ApplySel =>
                structBits.bitsWL(relWidth, relBitLow).anonymize.as(f.dfType)
            }
            val fieldVal = fields match {
              case _ if m.isAnonymous => addField.anonymize
              case _ : DFTuple.Fields => addField.setName(s"${m.name}${f.fieldName}")
              case _ : DFOpaque.Fields => addField.setName(s"${m.name}")
              case _ => addField.setName(s"${m.name}_${f.fieldName}")
            }
            fieldMemberMap += (m, f.fieldName) -> fieldVal
          }
        }
        List(m -> Patch.Add(dsn, Patch.Add.Config.After), m -> Patch.Remove)

      case bitsSel @ DFAny.Alias.BitsWL.Unref(_, _, relVal @ DFStruct(fields), relWidth, relBitLow, _, _) =>
        val fieldList = fields.getFieldList
        val dsn = new MetaDesign() {
          private def addConcat = {
            val concatBits = fieldList.map {
              case f : DFStruct.Field[DFAny.Type @unchecked] =>
                fieldMemberMap((relVal, f.fieldName)).asVal.bits.anonymize.asValOf[DFBits.Type[Int]]
            }.reduce[DFBits[Int]](_ ++ _)
            if (relWidth == relVal.width) concatBits else concatBits.anonymize.bitsWL(relWidth, relBitLow)
          }
          if (bitsSel.isAnonymous) addConcat.anonymize else addConcat.setName(bitsSel.name)
        }
        List(bitsSel -> Patch.Add(dsn, Patch.Add.Config.ReplaceWithLast()))
      case net @ DFNet.Unref(toVal @ DFStruct(fields), op, fromVal @ DFStruct(_), _, _) if !toVal.isInstanceOf[DFStruct.Selector] && !fromVal.isInstanceOf[DFStruct.Selector] =>
        val fieldList = fields.getFieldList
        val dsn = new MetaDesign() {
          fieldList.foreach {
            case f : DFStruct.Field[DFAny.Type @unchecked] =>
              DFNet(fieldMemberMap((toVal, f.fieldName)), op, fieldMemberMap((fromVal, f.fieldName)))
          }
        }
        List(net -> Patch.Add(dsn, Patch.Add.Config.After), net -> Patch.Remove)
      case _ => Nil
    }

    if (patchList.isEmpty) (false, c)
    else (true, c.newStage(designDB.patch(patchList)))
  }

  def flattenStruct : IRCompilation[D] = {
    var run = c.fixAnonymous.privFlattenStruct
    while (run._1) run = run._2.privFlattenStruct
    run._2
  }
}