package dfhdl.sim
import dfhdl.*

case class Pixel(
    r: UInt[8] <> VAL,
    g: UInt[8] <> VAL,
    b: UInt[8] <> VAL
) extends Struct

/** Nested composition: a struct holding another struct plus scalars. */
case class Packet(
    hdr: Bits[16] <> VAL,
    pix: Pixel <> VAL,
    valid: Bit <> VAL
) extends Struct

/** Struct DUT: whole-struct moves through ports, field reads, host-side nested construction,
  * field-partial RMW assignment, and whole-struct equality.
  */
class StructDut extends RTDesign:
  val i = Pixel <> IN
  val swapped = Pixel <> OUT
  val sumRG = UInt(8) <> OUT
  val isGray = Bit <> OUT
  val pkt = Packet <> OUT
  val v = Pixel <> VAR
  v := i
  v.r := i.b
  v.b := i.r
  swapped := v
  sumRG := i.r + i.g
  isGray := (i.r == i.g) & (i.g == i.b)
  pkt := Packet(hdr = h"BEEF", pix = i, valid = 1)
end StructDut

/** Vector DUT: whole-vector I/O, static and dynamic cell reads, and a dynamic cell write (RMW) over
  * a vector variable.
  */
class VecDut extends RTDesign:
  val vin = UInt(12) X 5 <> IN
  val idx = UInt(3) <> IN
  val cellIn = UInt(12) <> IN
  val cellOut = UInt(12) <> OUT
  val first = UInt(12) <> OUT
  val vout = UInt(12) X 5 <> OUT
  cellOut := vin(idx)
  first := vin(0)
  val v = UInt(12) X 5 <> VAR
  v := vin
  v(idx) := cellIn
  vout := v
end VecDut

/** Register-file DUT (docExamples RegFile shape, single domain): a vector of registers with a
  * conditional dynamic-index write and a dynamic-index read.
  */
class RegFileDut extends RTDesign:
  val waddr = UInt(3) <> IN
  val wdata = Bits(16) <> IN
  val wren = Bit <> IN
  val raddr = UInt(3) <> IN
  val rdata = Bits(16) <> OUT
  val regs = Bits(16) X 8 <> VAR.REG
  if (wren) regs(waddr).din := wdata
  rdata := regs(raddr)
end RegFileDut

/** Byte-enable RAM (the Servant firmware-RAM shape): 32-bit words with per-byte write enables (each
  * a masked sub-cell write to the same dynamic address) plus a dynamic-index async read. Exercises
  * the memory node's masked partial writes and read-first semantics on both tiers.
  */
class ByteMemDut extends RTDesign:
  val waddr = UInt(3) <> IN
  val wdata = Bits(32) <> IN
  val wsel = Bits(4) <> IN
  val raddr = UInt(3) <> IN
  val rdata = Bits(32) <> OUT
  val mem = Bits(32) X 8 <> VAR.REG
  if (wsel(0)) mem(waddr)(7, 0).din := wdata(7, 0)
  if (wsel(1)) mem(waddr)(15, 8).din := wdata(15, 8)
  if (wsel(2)) mem(waddr)(23, 16).din := wdata(23, 16)
  if (wsel(3)) mem(waddr)(31, 24).din := wdata(31, 24)
  rdata := mem(raddr)
end ByteMemDut

/** Combinational cell-wise wire vector (a pure combinational select, no register): every cell is
  * seeded, one cell is overwritten at a dynamic index, and one cell is read at a dynamic index.
  * Exercises the sweep-local scratch-array representation (version-threaded stores/loads).
  */
class CombVecDut extends RTDesign:
  val widx = UInt(3) <> IN
  val wdata = UInt(12) <> IN
  val ridx = UInt(3) <> IN
  val defv = UInt(12) <> IN
  val rdata = UInt(12) <> OUT
  val v = UInt(12) X 8 <> VAR
  for (i <- 0 until 8)
    COMB_LOOP // seed all cells combinationally
    v(i) := defv
  v(widx) := wdata // overwrite one cell at a dynamic index
  rdata := v(ridx) // read one cell at a dynamic index
end CombVecDut
