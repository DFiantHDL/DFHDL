package ZFiant.backend

import ZFiant._
import ZFiant.backend.utils._

package object vhdl {
  private val reservedKeywords : List[String] = List(
    "abs", "access", "after", "alias", "all", "and", "architecture", "array", "assert", "attribute", "begin",
    "block", "body", "buffer", "bus", "case", "component", "configuration", "constant", "disconnect", "downto",
    "else", "elsif", "end", "entity", "exit", "file", "for", "function", "generate", "generic", "group",
    "guarded", "if", "impure", "in", "inertial", "inout", "is", "label", "library", "linkage", "literal", "loop",
    "map", "mod", "nand", "new", "next", "nor", "not", "null", "of", "on", "open", "or", "others", "out",
    "package", "port", "postponed", "procedure", "process", "pure", "range", "record", "register", "reject",
    "rem", "report", "return", "rol", "ror", "select", "severity", "signal", "shared", "sla", "sll", "sra",
    "srl", "subtype", "then", "to", "transport", "type", "unaffected", "units", "until", "use", "variable",
    "wait", "when", "while", "with", "xnor", "xor",
  )

  object File {
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
          case p@DFAny.In() => from.name.toUpperCase
          case p@DFAny.Out() => from.name.toUpperCase
          case _ => from.name
        }
        ast.Name(nameDB.getUniqueName(modifiedName))
      }

    def fromDesign(design : DFDesign) : ast.File = {
      val designDB = design.db
      implicit val nameDB : NameDB = new NameDB(reservedKeywords, false)
      val members = designDB.ownerMemberTable(design.block)
      val ports = members.map {
        case p@DFAny.In() => ast.Value.Dcl.Port.In(p, p, None)
        case p@DFAny.Out() => ast.Value.Dcl.Port.Out(p, p, None)
      }
      ???
    }
  }
}
