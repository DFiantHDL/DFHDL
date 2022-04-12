package DFiant.compiler.stages.vhdl

import DFiant.compiler.stages.*
import DFiant.compiler.analysis.*
import DFiant.compiler.ir.*
import DFiant.compiler.printing.*

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

private case object VHDLUniqueNames extends UniqueNames(reservedKeywords, caseSensitive = false)
case object VHDLBackend extends Stage2:
  def dependencies: List[Stage2] = List(ToED, VHDLUniqueNames)
  def nullifies: Set[Stage2] = Set()
  def transform(designDB: DB)(using MemberGetSet): DB = designDB
end VHDLBackend

extension [T: HasDB](t: T)
  def getVHDLCode: String =
    val designDB = StageRunner.run(VHDLBackend)(t.db)
    given Printer = new RTPrinter(using designDB.getSet)
    designDB.codeString
  def printVHDLCode: DB =
    getVHDLCode
    t.db
