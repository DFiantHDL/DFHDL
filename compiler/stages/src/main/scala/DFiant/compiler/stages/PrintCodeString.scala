package DFiant.compiler.stages
import DFiant.compiler.ir.*
import DFiant.compiler.analysis.*
import DFiant.compiler.printing.*

case object PrintCodeString extends Stage:
  def dependencies: List[Stage] = List(UniqueDesigns, DFHDLUniqueNames)
  def nullifies: Set[Stage] = Set()
  def transform(designDB: DB)(using MemberGetSet): DB = designDB

extension [T: HasDB](t: T)
  def getCodeString: String =
    val designDB = StageRunner.run(PrintCodeString)(t.db)
    given Printer = DefaultPrinter(using designDB.getSet)
    designDB.codeString
  def printCodeString: DB =
    println(getCodeString)
    t.db
