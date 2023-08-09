package dfhdl.options
import dfhdl.options.PrinterOptions.*

final case class PrinterOptions(
    align: Align,
    color: Color
)
object PrinterOptions:

  // detecting if running in Scastie by checking the PWD
  private def inScastie: Boolean =
    System.getProperty("user.dir").startsWith("/tmp/scastie")

  // disabling color if in Scastie because of https://github.com/scalacenter/scastie/issues/492
  given default(using align: Align = true, color: Color = !inScastie): PrinterOptions =
    PrinterOptions(
      align = align,
      color = color
    )

  opaque type Align <: Boolean = Boolean
  given Conversion[Boolean, Align] = x => x

  opaque type Color <: Boolean = Boolean
  given Conversion[Boolean, Color] = x => x
end PrinterOptions
