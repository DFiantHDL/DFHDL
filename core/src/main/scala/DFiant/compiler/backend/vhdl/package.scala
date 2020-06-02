package DFiant
package compiler
package backend

import shapeless.HList


package object vhdl {
  private[vhdl] val reservedKeywords : Set[String] = Set(
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
  private[vhdl] implicit def getsetFromPrinter(implicit printer : Printer, lp : shapeless.LowPriority)
  : MemberGetSet = printer.getSet

  private implicit def VHDLBackend[D <: DFDesign, S <: shapeless.HList, C](c : C)(implicit conv : C => IRCompilation[D, S])
  : VHDLBackendOps[D, S] = new VHDLBackendOps[D, S](c)

  implicit object v93 extends Backend.Compiler[VHDLBackend[Revision.V93]] {
    def apply[D <: DFDesign, H <: HList](c : IRCompilation[D, H]) : Backend.Compilation[D, VHDLBackend[Revision.V93]] = c.vhdlCompile[Revision.V93]
  }
  implicit object v2008 extends Backend.Compiler[VHDLBackend[Revision.V2008]] {
    def apply[D <: DFDesign, H <: HList](c : IRCompilation[D, H]) : Backend.Compilation[D, VHDLBackend[Revision.V2008]] = c.vhdlCompile[Revision.V2008]
  }
}
