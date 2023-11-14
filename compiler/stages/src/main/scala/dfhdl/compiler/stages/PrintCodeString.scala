package dfhdl.compiler.stages
import dfhdl.compiler.ir.*
import dfhdl.compiler.analysis.*
import dfhdl.compiler.printing.*
import dfhdl.options.{CompilerOptions, PrinterOptions}

case object PrintCodeString extends Stage:
  def dependencies: List[Stage] =
    List(DropUnreferencedAnons, NamedAnonMultiref, UniqueDesigns, DFHDLUniqueNames)
  def nullifies: Set[Stage] = Set()
  def transform(designDB: DB)(using MemberGetSet, CompilerOptions): DB = designDB
end PrintCodeString

extension [T: HasDB](t: T)
  def getCodeString(align: Boolean)(using CompilerOptions): String =
    val designDB =
      StageRunner.run(PrintCodeString)(t.db)
    given PrinterOptions.Align = align
    val printer = new DFPrinter(using designDB.getSet)
    printer.csDB
  def getCodeString(using CompilerOptions): String = getCodeString(align = false)
  def printCodeString(using po: PrinterOptions, co: CompilerOptions): T =
    val designDB = StageRunner.run(PrintCodeString)(t.db)
    val printer = new DFPrinter(using designDB.getSet)
    println(printer.csDB)
    t
end extension
