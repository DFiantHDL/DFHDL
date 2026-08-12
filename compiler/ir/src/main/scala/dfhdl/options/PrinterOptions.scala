package dfhdl.options
import dfhdl.compiler.ir
import dfhdl.internals.simplePattenToRegex
import dfhdl.options.PrinterOptions.*
import dfhdl.internals.{scastieIsRunning, isColorTerminal}

final case class PrinterOptions(
    align: Align,
    color: Color,
    showGlobals: ShowGlobals,
    designPrintFilter: DesignPrintFilter,
    globalDefsFileName: GlobalDefsFileName
)
object PrinterOptions:
  opaque type Defaults[-T] <: PrinterOptions = PrinterOptions
  object Defaults:
    given (using
        align: Align,
        color: Color,
        showGlobals: ShowGlobals,
        designPrintFilter: DesignPrintFilter,
        globalDefsFileName: GlobalDefsFileName
    ): Defaults[Any] = PrinterOptions(
      align = align,
      color = color,
      showGlobals = showGlobals,
      designPrintFilter = designPrintFilter,
      globalDefsFileName = globalDefsFileName
    )
  given (using defaults: Defaults[Any]): PrinterOptions = defaults
  into opaque type Align <: Boolean = Boolean
  object Align:
    given Align = true
    given Conversion[Boolean, Align] = identity

  into opaque type Color <: Boolean = Boolean
  object Color:
    // disabling color if in Scastie because of https://github.com/scalacenter/scastie/issues/492,
    // and wherever the output is not going somewhere that renders ANSI (a pipe, a redirect, a log
    // file), which would otherwise litter the captured text with escape sequences
    given Color = !scastieIsRunning && isColorTerminal
    given Conversion[Boolean, Color] = identity

  into opaque type ShowGlobals <: Boolean = Boolean
  object ShowGlobals:
    given ShowGlobals = false
    given Conversion[Boolean, ShowGlobals] = identity

  into opaque type GlobalDefsFileName <: String = String
  object GlobalDefsFileName:
    given GlobalDefsFileName = ""
    given Conversion[String, GlobalDefsFileName] = identity

  trait DesignPrintFilter:
    def apply(design: ir.DFDesignBlock): Boolean

  object DesignPrintFilter:
    given DesignPrintFilter = All
    object All extends DesignPrintFilter:
      def apply(design: ir.DFDesignBlock): Boolean = true
    class Named(dclNamePattern: String) extends DesignPrintFilter:
      def apply(design: ir.DFDesignBlock): Boolean =
        dclNamePattern.simplePattenToRegex.matches(design.dclName)

end PrinterOptions
