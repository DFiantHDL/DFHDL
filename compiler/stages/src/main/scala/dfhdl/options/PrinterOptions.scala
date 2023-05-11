package dfhdl.options
import PrinterOptions.*

final case class PrinterOptions(
    align: Align,
    color: Color
)
object PrinterOptions:
  given default(using align: Align = true, color: Color = true): PrinterOptions = PrinterOptions(
    align = align,
    color = color
  )

  opaque type Align = Boolean
  given Conversion[Boolean, Align] = x => x
  given Conversion[Align, Boolean] = x => x

  opaque type Color = Boolean
  given Conversion[Boolean, Color] = x => x
  given Conversion[Color, Boolean] = x => x
