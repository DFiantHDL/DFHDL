package dfhdl.options
import dfhdl.compiler.ir
import dfhdl.internals.simplePattenToRegex
import dfhdl.options.PrinterOptions.*
import dfhdl.internals.scastieIsRunning

final case class PrinterOptions(
    align: Align,
    color: Color,
    showGlobals: ShowGlobals,
    designPrintFilter: DesignPrintFilter
)
object PrinterOptions:

  // disabling color if in Scastie because of https://github.com/scalacenter/scastie/issues/492
  given default(using
      align: Align,
      color: Color,
      showGlobals: ShowGlobals,
      designPrintFilter: DesignPrintFilter
  ): PrinterOptions =
    PrinterOptions(
      align = align,
      color = color,
      showGlobals = showGlobals,
      designPrintFilter = designPrintFilter
    )

  opaque type Align <: Boolean = Boolean
  object Align:
    given Align = true
    given Conversion[Boolean, Align] = identity

  opaque type Color <: Boolean = Boolean
  object Color:
    given Color = !scastieIsRunning
    given Conversion[Boolean, Color] = identity

  opaque type ShowGlobals <: Boolean = Boolean
  object ShowGlobals:
    given ShowGlobals = false
    given Conversion[Boolean, ShowGlobals] = identity

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
