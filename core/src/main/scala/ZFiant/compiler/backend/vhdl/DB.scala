package ZFiant
package compiler.backend.vhdl


object DB
//final case class DB(enums : Map[EnumType, adt.Value.Type.enumeration], files : Map[DFDesign.Block, adt.File])
//
//object DB {
//  def apply(designDB : DFDesign.DB) : DB = {
//    import designDB.__getset
//    val packName = globalNameDB(s"${designDB.top.typeName}_pack")
//    val enums = designDB.members.foldLeft(Map.empty[EnumType, adt.Value.Type.enumeration]) {
//      case (enumMap, member : DFAny) => member.dfType match {
//        case DFEnum.Type(enumType) if (!enumMap.contains(enumType)) =>
//          val vhdlEntries = enumType.entries.values.toList.map(entry => adt.Value.Type.enumeration.entry(globalNameDB(s"${enumType.name}_${entry.name}")))
//          val vhdlEnum = adt.Value.Type.enumeration(globalNameDB(enumType.name), enumType.width, vhdlEntries)
//          enumMap + (enumType -> vhdlEnum)
//        case _ => enumMap
//      }
//      case (enumMap, _) => enumMap
//    }
//
//    implicit class TokenExtension(token : DFAny.Token) {
//    }
//    implicit class DFMemberExtension(member : DFMember) {
//      def getVHDLName(implicit nameDB: NameDB[adt.Name]) : adt.Name = nameDB(member.name)
//    }
//    implicit class DFAnyExtension(member : DFAny) {
//      def getVHDLInit : Option[String] = member.tags.init match {
//        case Some(token :: _) if !token.isBubble => Some(token.getVHDLConst)
//        case _ => None
//      }
//    }
//    object RefMap {
//      private val refMap = mutable.Map.empty[DFMember, adt.HasName]
//      def add[V <: adt.Value](member : DFAny, value : V) : V = {
//        refMap += member -> value
//        value
//      }
//      def add(member : DFDesign.Block, value : adt.ComponentInstance) : adt.ComponentInstance = {
//        refMap += member -> value
//        value
//      }
//      def get(member : DFAny) : adt.Value = refMap(member).asInstanceOf[adt.Value]
//      def get(member : DFDesign.Block) : adt.ComponentInstance = refMap(member).asInstanceOf[adt.ComponentInstance]
//    }
//
//    val files = designDB.designMemberList.foldLeft(Map.empty[DFDesign.Block, adt.File]) {
//      case (designMap, (block, members)) =>
//        implicit val nameDB : NameDB[adt.Name] = globalNameDB.clone()
//        val namedValues = members.collect {
//          case v : DFAny if !v.isAnonymous =>
//            val modifier : adt.Value.ValueDcl.Modifier = v.modifier match {
//              case _ : DFAny.Modifier.Port.In => adt.Value.ValueDcl.Modifier.Port.In
//              case _ : DFAny.Modifier.Port.Out => adt.Value.ValueDcl.Modifier.Port.Out
//              case _ : DFAny.Modifier.NewVar =>
//                if (v.tags.customTags.contains(compiler.sync.SyncTag.Reg)) adt.Value.ValueDcl.Modifier.Signal
//                else if (designDB.getConnectionTo(v).isDefined) adt.Value.ValueDcl.Modifier.Signal
//                else adt.Value.ValueDcl.Modifier.Variable
//              case _ => adt.Value.ValueDcl.Modifier.Signal
//            }
//            RefMap.add(v, adt.Value.Dcl(modifier, v.getVHDLName, v.getVHDLType, v.getVHDLInit))
//        }
//        val ports = namedValues.collect {
//          case p @ adt.Value.ValueDcl.Port.In() => p
//          case p @ adt.Value.ValueDcl.Port.Out() => p
//        }
//        val signals = namedValues.collect {
//          case s @ adt.Value.ValueDcl.Signal() => s
//        }
//        val variables = namedValues.collect {
//          case v @ adt.Value.ValueDcl.Variable() => v
//        }
//        val componentInsts = members.collect {
//          case b : DFDesign.Block =>
//            val connections : List[(adt.Name, adt.Value)] = designDB.designMemberTable(b).collect {
//              case n @ DFNet.Connection(toRef, fromRef, _, _) if n.hasLateConstruction =>
//                val toVal = toRef.get
//                val fromVal = fromRef.get
//                if (toVal.isMemberOfDesign(b)) (RefMap.get(toVal).name, RefMap.get(fromVal))
//                else (RefMap.get(fromVal).name, RefMap.get(toVal))
//            }
//            adt.ComponentInstance(b.getVHDLName, ???, connections)
//        }
//
//        designMap
//      case (designMap, _) => designMap
//    }
//    ???
//  }
//}