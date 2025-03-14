package dfhdl.devices.tinytapeout.tt_vga_clock
import dfhdl.*

@top class FontROM(
    val FONT_FILE: String = "tt_vga_clock/font.list",
    val addr_width: Int = 6,
    val data_width: Int = 4
) extends RTDesign:
  val addr = UInt(addr_width) <> IN
  val dout = Bits(data_width) <> OUT.REG
  val memb =
    (Bits(data_width) X (1 << addr_width)) <> VAR initFile (FONT_FILE, InitFileFormat.VerilogBin)
  dout.din := memb(addr)
end FontROM
