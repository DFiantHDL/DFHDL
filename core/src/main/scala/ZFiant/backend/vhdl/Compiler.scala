package ZFiant.backend.vhdl

import ZFiant._
import ZFiant.backend.utils._

class Compiler(design : DFDesign) {
  private implicit def getVHDLType(from : DFAny) : ast.Value.Type = from.dfType match {
    case DFBits.Type(width) => ast.Value.Type.std_logic_vector(width)
    case DFUInt.Type(width) => ast.Value.Type.unsigned(width)
    case DFBool.Type() => ast.Value.Type.std_logic
  }
  private implicit def getName(from : DFAny)(implicit nameDB : NameDB) : ast.Name =
    if (from.isAnonymous) ast.Name.anonymous
    else {
      //port names are capitalized to make ports more visible
      val modifiedName = from match {
        case DFAny.In() => from.name.toUpperCase
        case DFAny.Out() => from.name.toUpperCase
        case _ => from.name
      }
      ast.Name(nameDB.getUniqueName(modifiedName))
    }

  private val designDB = design.db
  implicit val nameDB : NameDB = new NameDB(reservedKeywords, false)
  private val members = designDB.ownerMemberTable(design.block)
//  private val clkPort = ast.Clock()
  private val portMap : Map[DFAny, ast.Value] = Map.from(members.map {
    case p@DFAny.In() => p -> ast.Value.Dcl.Port.In(p, p, None)
    case p@DFAny.Out() => p -> ast.Value.Dcl.Port.Out(p, p, None)
  })


}
