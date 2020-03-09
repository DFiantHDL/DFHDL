package ZFiant
package compiler
package backend
package vhdl

import backend.utils._

final class Compiled(designDB : DFDesign.DB, block : DFDesign.Block) {
  import designDB.__getset
  private val nameDB : NameDB[adt.Name] = new NameDB[adt.Name](reservedKeywords, false, adt.Name(_))
  private val enumerations : Map[EnumType, adt.Value.Type.enumeration] = Map()
  private def getVHDLType(from : DFAny) : adt.Value.Type = from.dfType match {
    case DFBits.Type(width) => adt.Value.Type.std_logic_vector(width)
    case DFUInt.Type(width) => adt.Value.Type.unsigned(width)
    case DFSInt.Type(width) => adt.Value.Type.signed(width)
    case DFEnum.Type(enumType) => enumerations(enumType)
    case DFBool.Type() => adt.Value.Type.std_logic
    case _ => throw new IllegalArgumentException(s"\nUnsupported type for VHDL compilation. The variable ${from.getFullName} has type ${from.typeName}")
  }
  private def getVHDLName(from : DFMember) : adt.Name =
    if (from.isAnonymous) adt.Name.anonymous
    else {
      //port names are capitalized to make ports more visible
      val modifiedName = from match {
        case DFAny.In() => from.name.toUpperCase
        case DFAny.Out() => from.name.toUpperCase
        case _ => from.name
      }
      adt.Name(modifiedName)
    }

  private val members = designDB.ownerMemberTable(block)

  private val entityName = nameDB(block.typeName)
  private val archName = adt.Name(s"${entityName}_arch")

}

final class Compiler[D <: DFDesign, S <: shapeless.HList](c : Compilable[D, S]) {
  private val designDB = c.viaPortConnection.db

  def testVHDL : Compiled = new Compiled(designDB, designDB.top)
}
