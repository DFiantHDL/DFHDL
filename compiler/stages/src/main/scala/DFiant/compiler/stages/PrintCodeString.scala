package dfhdl.compiler.stages
import dfhdl.compiler.ir.*
import dfhdl.compiler.analysis.*
import dfhdl.compiler.printing.*

case object PrintCodeString extends Stage:
  def dependencies: List[Stage] = List(UniqueDesigns, DFHDLUniqueNames)
  def nullifies: Set[Stage] = Set()
  def transform(designDB: DB)(using MemberGetSet): DB = designDB
end PrintCodeString

extension [T: HasDB](t: T)
  def getCodeString(align: Boolean): String =
    val designDB = StageRunner.run(PrintCodeString)(t.db)
    given Printer = DefaultPrinter(using designDB.getSet)
    if (align)
      designDB.codeString
        .align("[ \\t]*val .*", "=", ".*<>.*")
        .align("[ \\t]*val .*", "<>", ".*")
        .align("[ \\t]*val .*<>.*", "init", ".*")
        .align("[ ]*[a-zA-Z0-9_.]+[ ]*", ":=|<>|:==", ".*")
    else designDB.codeString
  def getCodeString: String = getCodeString(align = false)
  def printCodeString: DB =
    println(getCodeString(align = true))
    t.db
end extension
