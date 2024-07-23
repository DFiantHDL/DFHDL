package dfhdl.options
import dfhdl.options.PrinterOptions.*

final case class PrinterOptions(
    align: Align,
    color: Color,
    showGlobals: ShowGlobals
)
object PrinterOptions:

  // detecting if running in Scastie by checking the PWD
  private def inScastie: Boolean =
    System.getProperty("user.dir").startsWith("/tmp/scastie")

  // disabling color if in Scastie because of https://github.com/scalacenter/scastie/issues/492
  given default(using
      align: Align,
      color: Color,
      showGlobals: ShowGlobals
  ): PrinterOptions =
    PrinterOptions(
      align = align,
      color = color,
      showGlobals = showGlobals
    )

  opaque type Align <: Boolean = Boolean
  object Align:
    given Align = true
    given Conversion[Boolean, Align] = identity

  opaque type Color <: Boolean = Boolean
  object Color:
    given Color = !inScastie
    given Conversion[Boolean, Color] = identity

  opaque type ShowGlobals <: Boolean = Boolean
  object ShowGlobals:
    given ShowGlobals = false
    given Conversion[Boolean, ShowGlobals] = identity
end PrinterOptions
