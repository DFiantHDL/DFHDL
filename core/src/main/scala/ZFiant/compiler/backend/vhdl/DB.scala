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
    import designDB.__getset
    val globalNameDB : NameDB[adt.Name] = new NameDB[adt.Name](reservedKeywords, false, adt.Name(_))

    val enums = designDB.members.foldLeft(Map.empty[Enum, adt.Value.Type.enumeration]) {
      case (enumMap, member : DFAny) => member.dfType match {
        case DFEnum.Type(enumType) if (!enumMap.contains(enumType)) =>
          val vhdlEntries = enumType.entries.values.toList.map(entry => adt.Value.Type.enumeration.entry(globalNameDB(s"${enumType.name}_${entry.name}")))
          val vhdlEnum = adt.Value.Type.enumeration(globalNameDB(enumType.name), enumType.width, vhdlEntries)
          enumMap + (enumType -> vhdlEnum)
        case _ => enumMap
      }
      case (enumMap, _) => enumMap
    }

    implicit class TokenExtension(token : DFAny.Token) {
      def getVHDLConst : String = token match {
        case x : DFBits.Token[_] => if (x.width % 4 == 0) s"""x"${x.value.toHex}"""" else s""""${x.value.toBin}""""
        case x : DFUInt.Token[_] => s"""${x.width}d"${x.value}""""
        case x : DFSInt.Token[_] => s"""${x.width}d"${x.value}""""
        case x : DFBool.Token => if (x.value) "'1'" else "'0'"
        case x : DFEnum.Token[_] => ??? //enums(x.enumType).entries(x.enumType.entries(x.value.get.value))
        case _ => ??? //throw new IllegalArgumentException(s"\nUnsupported type for VHDL compilation. The variable ${member.fullName} has type ${member.typeName}")
      }
    }
    implicit class DFMemberExtension(member : DFMember) {
      def getVHDLName(implicit nameDB: NameDB[adt.Name]) : adt.Name = nameDB(member.name)
    }
    implicit class DFAnyExtension(member : DFAny) {
      def getVHDLType : adt.Value.Type = member.dfType match {
        case DFBits.Type(width) => adt.Value.Type.std_logic_vector(width)
        case DFUInt.Type(width) => adt.Value.Type.unsigned(width)
        case DFSInt.Type(width) => adt.Value.Type.signed(width)
        case DFEnum.Type(enumType) => enums(enumType)
        case DFBool.Type() => adt.Value.Type.std_logic
        case _ => throw new IllegalArgumentException(s"\nUnsupported type for VHDL compilation. The variable ${member.getFullName} has type ${member.typeName}")
      }
      def getVHDLInit : Option[String] = member.tags.init.map(i => i.head.getVHDLConst)
    }

    val files = designDB.designMemberList.foldLeft(Map.empty[DFDesign.Block, adt.File]) {
      case (designMap, (block, members)) =>
        implicit val nameDB : NameDB[adt.Name] = globalNameDB.clone()
        val portMap : Map[DFAny, adt.Value.Dcl[adt.Value.Dcl.Modifier.Port]] = Map.from(members.collect {
          case p@DFAny.In() => p -> adt.Value.Dcl.Port.In(p.getVHDLName, p.getVHDLType, p.getVHDLInit)
          case p@DFAny.Out() => p -> adt.Value.Dcl.Port.Out(p.getVHDLName, p.getVHDLType, p.getVHDLInit)
        })

        designMap
      case (designMap, _) => designMap
    }
    ???
  }
}