import DFiant.*

/*
    // draw paddles - are paddles at current screen position?
    always_comb begin
        p1_draw = (sx >= P_OFFS) && (sx < P_OFFS + P_W)
               && (sy >= p1y) && (sy < p1y + P_H);
        p2_draw = (sx >= H_RES - P_OFFS - P_W) && (sx < H_RES - P_OFFS)
               && (sy >= p2y) && (sy < p2y + P_H);
    end

    // paddle collision detection
    always_ff @(posedge clk_pix) begin
        if (animate) begin
            p1_col <= 0;
            p2_col <= 0;
        end else if (b_draw) begin
            if (p1_draw) p1_col <= 1;
            if (p2_draw) p2_col <= 1;
        end
    end

 */

class Example(using DFC) extends RTDesign:
  val animate, b_draw, p1_draw = DFBool <> VAR
  val P_H = 40 // height in pixels
  val P_W = 10 // width in pixels
  val P_SP = 4 // speed
  val P_OFFS = 32 // offset from screen edge
  val CORDW = 10
  val sx, sy, p1y = DFUInt(CORDW) <> REG
  val p1_col, p1_col_r = DFBool <> REG

  p1_draw := (sx >= P_OFFS) && (sx < P_OFFS + P_W) && (sy >= p1y) && (sy < p1y + P_H)

  if (animate)
    p1_col.din := 0
  else if (b_draw)
    if (p1_draw) p1_col.din := 1

  p1_col.din := p1_col.reg
  p1_col.reg

  val a, b = DFBool <> VAR

  val c = a.reg
  a := b.reg
  b := c

end Example
