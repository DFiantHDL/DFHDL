package dfhdl.compiler.stages
import dfhdl.compiler.ir.*
import dfhdl.compiler.analysis.*
import dfhdl.compiler.printing.*
import dfhdl.options.{CompilerOptions, PrinterOptions}

// A no-op dependency anchor: running it forces DropUnreferencedAnons,
// NamedAnonMultiref, and DFHDLUniqueNames to run so the DB is in printable
// shape, then the `getCodeString`/`printCodeString` extensions run the printer.
case object PrintCodeString
    extends BundleStage(DropUnreferencedAnons, NamedAnonMultiref, DFHDLUniqueNames)

extension [T: HasDB](t: T)
  def getCodeString(align: Boolean)(using CompilerOptions): String =
    given PrinterOptions.Align = align
    val designDB =
      StageRunner.run(PrintCodeString)(t.db)
    // flatten the hierarchical pipeline output for the (flat-only) printer.
    // TODO: temporary — remove once the backend/DFHDL printers are hardened for
    // the hierarchical root DB; until then the printer always renders from flat.
    val printer = new DFPrinter(using designDB.newToOld.getSet)
    printer.csDB
  def getCodeString(using CompilerOptions): String = getCodeString(align = false)
  def getCompiledCodeString(using po: PrinterOptions, co: CompilerOptions): String =
    co.backend.printer(t.db).csDB
  def printCodeString(using po: PrinterOptions, co: CompilerOptions): T =
    val designDB = StageRunner.run(PrintCodeString)(t.db)
    // flatten the hierarchical pipeline output for the (flat-only) printer.
    // TODO: temporary — remove once the backend/DFHDL printers are hardened for
    // the hierarchical root DB; until then the printer always renders from flat.
    val printer = new DFPrinter(using designDB.newToOld.getSet)
    println(printer.csDB)
    t
end extension
