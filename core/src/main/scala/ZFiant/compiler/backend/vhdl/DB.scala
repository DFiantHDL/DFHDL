package ZFiant
package compiler.backend.vhdl

import collection.mutable
import compiler.backend.utils.NameDB

final case class DB(enums : Map[Enum, adt.Value.Type.enumeration], files : Map[DFDesign.Block, adt.File])

object DB {
  final case class Body()
  object bodyDB extends DFiant.internals.DSLOwnerConstruct.DB[DFDesign.Block, Body] {
    override def ownerToString(ownerTypeName: String, ownerBody: Body): String = ???
  }
  def apply(designDB : DFDesign.DB) : DB = {
    val nameDB : NameDB[adt.Name] = new NameDB[adt.Name](reservedKeywords, false, adt.Name(_))

    val enums = designDB.members.foldLeft(Map.empty[Enum, adt.Value.Type.enumeration]) {
      case (enumMap, member : DFAny) => member.dfType match {
        case DFEnum.Type(enumType) if (!enumMap.contains(enumType)) =>
          val vhdlEntries = enumType.entries.values.toList.map(entry => adt.Value.Type.enumeration.entry(nameDB(s"${enumType.name}_${entry.name}")))
          val vhdlEnum = adt.Value.Type.enumeration(nameDB(enumType.name), enumType.width, vhdlEntries)
          enumMap + (enumType -> vhdlEnum)
        case _ => enumMap
      }
      case (enumMap, _) => enumMap
    }

//    final case class FileBody(ports : )

    val files = designDB.designMemberList.foldLeft(Map.empty[DFDesign.Block, adt.File]) {
      case (designMap, (block, members)) =>
        designMap
      case (designMap, _) => designMap
    }
    ???
  }
}