package dfhdl.compiler.stages.vhdl

import dfhdl.compiler.stages.*
import dfhdl.compiler.analysis.*
import dfhdl.compiler.ir.*
import dfhdl.compiler.printing.*

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
case object VHDLBackend extends Stage:
  def dependencies: List[Stage] = List(ToED, VHDLUniqueNames)
  def nullifies: Set[Stage] = Set()
  def transform(designDB: DB)(using MemberGetSet): DB = designDB
end VHDLBackend

extension [T: HasDB](t: T)
  def getVHDLCode(align: Boolean): String =
    val designDB = StageRunner.run(VHDLBackend)(t.db)
    given Printer = new VHDLPrinter(using designDB.getSet)
    if (align)
      designDB.codeString
        .align(".*", ":", "[ ]*(?:in|out|inout) .*")
        .align(".*:[ ]*(?:in|out|inout)", " ", ".*")
        .align("[ ]*(?:signal|variable|constant) .*", ": ", ".*")
        .align("[ ]*[a-zA-Z0-9_.]+[ ]*", ":=|<=", ".*")
    else designDB.codeString
  def getVHDLCode: String = getVHDLCode(align = false)
  def printVHDLCode: DB =
    getVHDLCode(align = true)
    t.db
end extension
