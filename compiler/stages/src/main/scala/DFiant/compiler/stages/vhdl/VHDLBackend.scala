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

private class VHDLBackend(db: DB) extends Stage(db):
  override protected def preTransform: DB =
    val updatedDB = db /*.dropUnreferenced*/ .uniqueDesigns
    val designNames = updatedDB.members.collect { case block: DFDesignBlock => block.dclName }
    updatedDB
      .uniqueNames(designNames.toSet ++ reservedKeywords, caseSensitive = false)
      .viaConnection
      .toED
  override def transform: DB =
    println(toVHDL)
    designDB
  def toVHDL: String =
    given Printer = new RTPrinter
    designDB.codeString
end VHDLBackend

extension [T: HasDB](t: T)
  def printVHDLCode: DB = new VHDLBackend(t.db).transform
  def getVHDLCode: String = new VHDLBackend(t.db).toVHDL
