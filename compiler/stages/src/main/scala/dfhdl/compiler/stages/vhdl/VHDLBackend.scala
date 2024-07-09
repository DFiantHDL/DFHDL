package dfhdl.compiler.stages.vhdl

import dfhdl.compiler.stages.*
import dfhdl.compiler.analysis.*
import dfhdl.compiler.ir.*
import dfhdl.compiler.printing.*
import dfhdl.options.{CompilerOptions, PrinterOptions}

private val reservedKeywords: Set[String] = Set(
  "abs", "access", "after", "alias", "all", "and", "architecture", "array", "assert", "attribute",
  "begin", "block", "body", "buffer", "bus", "case", "component", "configuration", "constant",
  "disconnect", "downto", "else", "elsif", "end", "entity", "exit", "file", "for", "function",
  "generate", "generic", "group", "guarded", "if", "impure", "in", "inertial", "inout", "is",
  "label", "library", "linkage", "literal", "loop", "map", "mod", "nand", "new", "next", "nor",
  "not", "null", "of", "on", "open", "or", "others", "out", "package", "port", "postponed",
  "procedure", "process", "pure", "range", "record", "register", "reject", "rem", "report",
  "return", "rol", "ror", "select", "severity", "signal", "shared", "sla", "sll", "sra", "srl",
  "subtype", "then", "to", "transport", "type", "unaffected", "units", "until", "use", "variable",
  "wait", "when", "while", "with", "xnor", "xor"
)

enum VHDLDialect derives CanEqual:
  case v93, v2008, v2019

private case object VHDLUniqueNames extends UniqueNames(reservedKeywords, caseSensitive = false)
case object VHDLBackend extends Stage:
  def dependencies: List[Stage] =
    List(DropUnreferencedAnons, NamedAnonMultiref, ToED, DropDomains, DropMagnets,
      ExplicitNamedVars, DropCondDcls, SimpleOrderMembers, VHDLUniqueNames, ViaConnection)
  def nullifies: Set[Stage] = Set()
  def transform(designDB: DB)(using MemberGetSet, CompilerOptions): DB = designDB
end VHDLBackend
