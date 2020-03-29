package ZFiant
package compiler
package backend

import ZFiant.compiler.Compilable.Cmd


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

  implicit def VHDLBackend[D <: DFDesign, S <: shapeless.HList, C](c : C)(implicit conv : C => Compilable[D, S])
  : VHDLBackend[D, S] = new VHDLBackend[D, S](c)

  import shapeless.{:: => #:}
  sealed implicit class VHDLCompiled[D <: DFDesign, S <: shapeless.HList](c : Compilable[D, VHDLCompiler #: S]) {
    def getFileNames : List[String] = c.cmdSeq.collect {
      case Cmd.GenFile(fileName, _) if fileName.endsWith(".vhdl") => fileName
    }.toList
  }
}
