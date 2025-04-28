package dfhdl.devices.tinytapeout.tt_vga_clock
import dfhdl.*
@top class ButtonPulse(
    val MAX_COUNT: Int = 8, // max wait before issue next pulse
    val DEC_COUNT: Int = 2, // every pulse, decrement comparitor by this amount
    val MIN_COUNT: Int = 1 // until reaches this wait time
) extends RTDesign:
  val clk_en = Bit <> IN
  val button = Bit <> IN
  val pulse = Bit <> OUT

  val comp = UInt.until(MAX_COUNT) <> VAR.REG init (MAX_COUNT - 1)
  val count = UInt.until(MAX_COUNT) <> VAR.REG init 0

  if (clk_en)
    if (button)
      count.din := count + 1;

    // if button is held, increase pulse rate by reducing comp
    if (count == 0 && comp > (MIN_COUNT + DEC_COUNT))
      comp.din := comp - DEC_COUNT;

    // reset counter
    if (count == comp)
      count.din := 0;

    // if button is released, set count and comp to default
    if (!button)
      count.din := 0;
      comp.din := MAX_COUNT - 1;
    end if
  end if
  pulse <> (clk_en && button && count == 0)
end ButtonPulse
