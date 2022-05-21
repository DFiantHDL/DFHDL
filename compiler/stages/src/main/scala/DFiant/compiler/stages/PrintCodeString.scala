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
  def getCodeString(fixAlign: Boolean): String =
    val designDB = StageRunner.run(PrintCodeString)(t.db)
    given Printer = DefaultPrinter(using designDB.getSet)
    if (fixAlign)
      designDB.codeString
        .align("[ \\t]*val .*", "=", ".*<>.*")
        .align("[ \\t]*val .*", "<>", ".*")
        .align("[ \\t]*val .*<>.*", "init", ".*")
        .align("[ ]*[a-zA-Z0-9_.]+[ ]*", ":=|<>|:==", ".*")
    else designDB.codeString
  def getCodeString: String = getCodeString(fixAlign = false)
  def printCodeString: DB =
    println(getCodeString(fixAlign = true))
    t.db
end extension
