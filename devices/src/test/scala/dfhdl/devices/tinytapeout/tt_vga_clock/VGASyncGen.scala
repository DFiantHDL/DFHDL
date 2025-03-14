package dfhdl.devices.tinytapeout.tt_vga_clock
import dfhdl.*

@top class VGASyncGen extends RTDesign:
  val hsync = Bit <> OUT
  val vsync = Bit <> OUT
  val x_px = UInt(10) <> OUT.REG init 0
  val y_px = UInt(10) <> OUT.REG init 0
  val activevideo = Bit <> OUT

  // Video structure constants.
  val activeHvideo: Int = 640 // Width of visible pixels.
  val activeVvideo: Int = 480 // Height of visible lines.
  val hfp: Int = 24 // Horizontal front porch length.
  val hpulse: Int = 40 // Hsync pulse length.
  val hbp: Int = 128 // Horizontal back porch length.
  val vfp: Int = 9 // Vertical front porch length.
  val vpulse: Int = 3 // Vsync pulse length.
  val vbp: Int = 28 // Vertical back porch length.
  val blackH: Int = hfp + hpulse + hbp // Hide pixels in one line.
  val blackV: Int = vfp + vpulse + vbp // Hide lines in one frame.
  val hpixels: Int = blackH + activeHvideo // Total horizontal pixels.
  val vlines: Int = blackV + activeVvideo // Total lines.

  // Registers for storing the horizontal & vertical counters.
  val hc = UInt(10) <> VAR.REG init 0
  val vc = UInt(10) <> VAR.REG init 0

  // Keep counting until the end of the line.
  if (hc < hpixels - 1) hc.din := hc + 1;
  else
    // When we hit the end of the line, reset the horizontal
    // counter and increment the vertical counter.
    // If vertical counter is at the end of the frame, then
    // reset that one too.
    hc.din := 0;
    if (vc < vlines - 1)
      vc.din := vc + 1;
    else
      vc.din := 0;
    end if
  end if

  // Generate sync pulses (active low) and active video.
  hsync <> !(hc >= hfp && hc < hfp + hpulse)
  vsync <> !(vc >= vfp && vc < vfp + vpulse)
  activevideo <> (hc >= blackH && vc >= blackV)

  // Generate color.
  x_px.din := hc - blackH
  y_px.din := vc - blackV
end VGASyncGen
