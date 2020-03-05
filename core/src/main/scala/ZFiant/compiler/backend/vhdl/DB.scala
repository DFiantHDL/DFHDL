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
    val packName = globalNameDB(s"${designDB.top.typeName}_pack")
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
        case x : DFEnum.Token[_] => s"${x.enumType.name}_${x.value.get.name}"
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
      def getVHDLInit : Option[String] = member.tags.init match {
        case Some(token :: _) if !token.isBubble => Some(token.getVHDLConst)
        case _ => None
      }
    }
    object RefMap {
      private val refMap = mutable.Map.empty[DFMember, adt.HasName]
      def add[V <: adt.Value](member : DFAny, value : V) : V = {
        refMap += member -> value
        value
      }
      def add(member : DFDesign.Block, value : adt.ComponentInstance) : adt.ComponentInstance = {
        refMap += member -> value
        value
      }
      def get(member : DFAny) : adt.Value = refMap(member).asInstanceOf[adt.Value]
      def get(member : DFDesign.Block) : adt.ComponentInstance = refMap(member).asInstanceOf[adt.ComponentInstance]
    }

    val files = designDB.designMemberList.foldLeft(Map.empty[DFDesign.Block, adt.File]) {
      case (designMap, (block, members)) =>
        implicit val nameDB : NameDB[adt.Name] = globalNameDB.clone()
        val namedValues = members.collect {
          case v : DFAny if !v.isAnonymous =>
            val modifier : adt.Value.Dcl.Modifier = v.modifier match {
              case _ : DFAny.Modifier.Port.In => adt.Value.Dcl.Modifier.Port.In
              case _ : DFAny.Modifier.Port.Out => adt.Value.Dcl.Modifier.Port.Out
              case _ : DFAny.Modifier.NewVar =>
                if (designDB.getConnectionTo(v).isDefined) adt.Value.Dcl.Modifier.Signal
                else adt.Value.Dcl.Modifier.Variable
              case _ => adt.Value.Dcl.Modifier.Signal
            }
            RefMap.add(v, adt.Value.Dcl(modifier, v.getVHDLName, v.getVHDLType, v.getVHDLInit))
        }
        val ports = namedValues.collect {
          case p @ adt.Value.Dcl.Port.In() => p
          case p @ adt.Value.Dcl.Port.Out() => p
        }
        val signals = namedValues.collect {
          case s @ adt.Value.Dcl.Signal() => s
        }
        val variables = namedValues.collect {
          case v @ adt.Value.Dcl.Variable() => v
        }
        val componentInsts = members.collect {
          case b : DFDesign.Block =>
            val connections : List[(adt.Name, adt.Value)] = designDB.designMemberTable(b).collect {
              case n @ DFNet.Connection(toRef, fromRef, _, _) if n.hasLateConstruction =>
                val toVal = toRef.get
                val fromVal = fromRef.get
                if (toVal.isMemberOfDesign(b)) (RefMap.get(toVal).name, RefMap.get(fromVal))
                else (RefMap.get(fromVal).name, RefMap.get(toVal))
            }
            adt.ComponentInstance(b.getVHDLName, ???, connections)
        }

        designMap
      case (designMap, _) => designMap
    }
    ???
  }
}