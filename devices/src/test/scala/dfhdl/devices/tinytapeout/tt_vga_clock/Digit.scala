package dfhdl.devices.tinytapeout.tt_vga_clock
import dfhdl.*

@top class Digit(
    val DIGIT_INDEX_FILE: String = "tt_vga_clock/digit_index.hex",
    val COL_INDEX_FILE: String = "tt_vga_clock/col_index.hex",
    val COLOR_INDEX_FILE: String = "tt_vga_clock/color.bin",
    val FONT_W: Int = 4,
    val FONT_H: Int = 5,
    val NUM_BLOCKS: Int = 32
) extends RTDesign:
  // val COL_INDEX_W = $clog2(FONT_W);
  val x_block = UInt(6) <> IN

  /** the number to display: [0->9] */
  val number = UInt.to(9) <> IN

  /** shift through the colours */
  val color_offset = UInt(4) <> IN
  val digit_index = UInt(6) <> OUT.REG
  val color = UInt(6) <> OUT.REG
  val col_index = UInt.until(FONT_W) <> OUT.REG

  val digit_index_mem =
    UInt(6) X 12 <> VAR initFile (DIGIT_INDEX_FILE, InitFileFormat.VerilogHex)
  val col_index_mem =
    UInt.until(FONT_W) X NUM_BLOCKS <> VAR initFile (COL_INDEX_FILE, InitFileFormat.VerilogHex)
  val color_index_mem =
    UInt(6) X 8 <> VAR initFile (COLOR_INDEX_FILE, InitFileFormat.VerilogBin)

  val char = x_block.bits(5, 2)
  digit_index.din := digit_index_mem(number)
  val col_index_mem_addr: UInt[5] <> VAL = (x_block min (NUM_BLOCKS - 1)).truncate
  col_index.din := col_index_mem(col_index_mem_addr)
  color.din := color_index_mem((char + color_offset).resize(3))
end Digit
