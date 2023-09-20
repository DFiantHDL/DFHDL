package dfhdl.compiler.stages
import dfhdl.compiler.ir.*
import dfhdl.compiler.analysis.*
import dfhdl.compiler.printing.*
import dfhdl.options.{CompilerOptions, PrinterOptions}

case object PrintCodeString extends Stage:
  object Coloring:
    import io.AnsiColor._
    val LIT: String = "\u001B[38;5;5m"
    val scalaKWColor: String = s"$BLUE$BOLD"
    val scalaKW: Set[String] =
      Set("class", "end", "enum", "extends", "new", "object", "val", "if", "else", "match", "case")
    val dfhdlKWColor: String = s"$MAGENTA$BOLD"
    val dfhdlKW: Set[String] =
      Set("VAR", "REG", "WIRE", "IN", "OUT", "INOUT", "VAL", "DFDesign", "RTDesign", "EDDesign",
        "DFDomain", "RTDomain", "EDDomain", "process", "forever", "all")
    val dfhdlOps: Set[String] = Set("<>", ":=", ":==")
    val dfhdlTypes: Set[String] =
      Set("Bit", "Boolean", "UInt", "SInt", "Bits", "X", "Encode", "Struct", "Opaque", "StartAt",
        "OneHot", "Grey")
    val dfhdlTPColor: String = "\u001B[38;5;94m"
    val OP: String = s"$BOLD"
    val FN: String = "\u001B[38;5;54m"
    val TP: String = "\u001B[38;5;94m"
    extension (text: String)
      def color: String =
        text
          .colorWords(scalaKW, scalaKWColor)
          .colorWords(dfhdlKW, dfhdlKWColor)
          .colorOps(dfhdlOps, dfhdlKWColor)
          .colorWords(dfhdlTypes, dfhdlTPColor)
  end Coloring
  def dependencies: List[Stage] =
    List(DropUnreferencedAnons, NamedAnonMultiref, UniqueDesigns, DFHDLUniqueNames)
  def nullifies: Set[Stage] = Set()
  def transform(designDB: DB)(using MemberGetSet): DB = designDB
end PrintCodeString

extension [T: HasDB](t: T)
  def getCodeString(align: Boolean): String =
    val designDB =
      StageRunner.run(PrintCodeString)(t.db)(using dfhdl.options.CompilerOptions.default)
    given PrinterOptions.Align = align
    val printer = new DFPrinter(using designDB.getSet)
    printer.csDB
  def getCodeString: String = getCodeString(align = false)
  def printCodeString(using po: PrinterOptions, co: CompilerOptions): T =
    val designDB = StageRunner.run(PrintCodeString)(t.db)
    val printer = new DFPrinter(using designDB.getSet)
    println(printer.csDB)
    t
end extension
