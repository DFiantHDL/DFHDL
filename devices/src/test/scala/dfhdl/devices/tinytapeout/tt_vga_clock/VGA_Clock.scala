package dfhdl.devices.tinytapeout.tt_vga_clock
import dfhdl.*

@top class VGA_Clock extends RTDesign:
  val adj_hrs = Bit <> IN
  val adj_min = Bit <> IN
  val adj_sec = Bit <> IN
  val hsync = Bit <> OUT
  val vsync = Bit <> OUT
  val rrggbb = Bits(6) <> OUT

  val sec_u = UInt(4) <> VAR.REG init 0
  val sec_d = UInt(3) <> VAR.REG init 0
  val min_u = UInt(4) <> VAR.REG init 0
  val min_d = UInt(3) <> VAR.REG init 0
  val hrs_u = UInt(4) <> VAR.REG init 0
  val hrs_d = UInt(2) <> VAR.REG init 0
  val sec_counter = UInt(26) <> VAR.REG init 0

  val OFFSET_Y_BLK = 0;
  val OFFSET_X_BLK = 1;
  val NUM_CHARS = 8;
  val FONT_W = 4;
  val FONT_H = 5;
  val COLON = 10;
  val BLANK = 11;
  val COL_INDEX_W = clog2(FONT_W);
  val MAX_BUT_RATE = 16;
  val DEC_COUNT = 1;
  val MIN_COUNT = 2;

  val color_offset = UInt(4) <> VAR.REG init 0
  val number = UInt(4) <> VAR

  val vga = VGASyncGen()
  vga.hsync <> hsync
  vga.vsync <> vsync
  val x_block = ((vga.x_px - 64) >> 4).resize(6)
  val y_block = ((vga.y_px - 200) >> 4).resize(6)
  val but_clk_en = vga.y_px == 0 && vga.x_px == 0

  val pulse_sec =
    ButtonPulse(MAX_COUNT = MAX_BUT_RATE, DEC_COUNT = DEC_COUNT, MIN_COUNT = MIN_COUNT)
  val pulse_min =
    ButtonPulse(MAX_COUNT = MAX_BUT_RATE, DEC_COUNT = DEC_COUNT, MIN_COUNT = MIN_COUNT)
  val pulse_hrs =
    ButtonPulse(MAX_COUNT = MAX_BUT_RATE, DEC_COUNT = DEC_COUNT, MIN_COUNT = MIN_COUNT)

  pulse_sec.clk_en <> but_clk_en
  pulse_sec.button <> adj_sec
  pulse_min.clk_en <> but_clk_en
  pulse_min.button <> adj_min
  pulse_hrs.clk_en <> but_clk_en
  pulse_hrs.button <> adj_hrs

  if (sec_u == 10)
    sec_u.din := 0;
    sec_d.din := sec_d + 1;
  end if
  if (sec_d == 6)
    sec_d.din := 0;
    min_u.din := min_u + 1;
    color_offset.din := color_offset + 1;
  end if
  if (min_u == 10)
    min_u.din := 0;
    min_d.din := min_d + 1;
  end if
  if (min_d == 6)
    min_d.din := 0;
    hrs_u.din := hrs_u + 1;
  end if
  if (hrs_u == 10)
    hrs_u.din := 0;
    hrs_d.din := hrs_d + 1;
  end if
  if (hrs_d == 2 && hrs_u == 4)
    hrs_u.din := 0;
    hrs_d.din := 0;
  end if
  // second counter
  sec_counter.din := sec_counter + 1;
  if (sec_counter == 31_500_000)
    sec_u.din := sec_u + 1;
    sec_counter.din := 0;
  end if
  // adjustment buttons
  if (pulse_sec.pulse)
    sec_u.din := sec_u + 1;
  if (pulse_min.pulse)
    min_u.din := min_u + 1;
    color_offset.din := color_offset + 1;
  end if
  if (pulse_hrs.pulse)
    hrs_u.din := hrs_u + 1;
  end if

  // TODO
  number := 0

  val digit = Digit(
    FONT_W = FONT_W,
    FONT_H = FONT_H,
    NUM_BLOCKS = NUM_CHARS * FONT_W
  )
  digit.x_block <> x_block
  digit.number <> number
  digit.color_offset <> color_offset

  val font = FontROM(data_width = FONT_W)
  font.addr <> (digit.digit_index + y_block)

  val x_block_q = x_block.reg(1, init = ?)
  val y_block_q = y_block.reg(1, init = ?)
  val col_index_q = digit.col_index.reg(1, init = ?)
  val draw = Bit <> VAR.REG init 0
  val font_out_idx = (FONT_W - 1) - col_index_q
  rrggbb <> (vga.activevideo && draw).sel(digit.color, all(0))
  if (x_block_q < FONT_W * NUM_CHARS && y_block_q < FONT_H)
    draw.din := font.dout(font_out_idx)
  else
    draw.din := 0
  end if
end VGA_Clock
