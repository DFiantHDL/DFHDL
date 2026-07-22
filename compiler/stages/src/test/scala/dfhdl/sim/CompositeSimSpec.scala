package dfhdl.sim
import dfhdl.*

/** Composite-type (struct/vector) engine coverage on both kernel tiers: packed-bits lowering of
  * struct fields and vector cells, typed peek/poke of whole composite values, dynamic indexing, and
  * a register file with dynamic writes.
  */
class CompositeSimSpec extends SimSpec:
  bothTiers("struct fields, nesting, and RMW field assignment"): tier =>
    (new StructDut).simulation { dut =>
      val pix = Pixel(r = 17, g = 42, b = 99)
      dut.i.poke(pix)
      assertEquals(dut.swapped.peek, Pixel(r = 99, g = 42, b = 17))
      assertEquals(dut.sumRG.peek, 59)
      assertEquals(dut.isGray.peek, 0)
      assertEquals(dut.pkt.peek, Packet(hdr = h"BEEF", pix = pix, valid = 1))
      val gray = Pixel(r = 7, g = 7, b = 7)
      dut.i.poke(gray)
      assertEquals(dut.isGray.peek, 1)
      assertEquals(dut.swapped.peek, gray)
    }.withTier(tier).run()

  bothTiers("vector cells: whole I/O, static/dynamic reads, dynamic RMW write"): tier =>
    (new VecDut).simulation { dut =>
      val cells = Vector(d"12'100", d"12'200", d"12'300", d"12'400", d"12'500")
      dut.vin.poke(cells)
      dut.cellIn.poke(d"12'4095")
      assertEquals(dut.first.peek, cells(0))
      for i <- 0 until 5 do
        dut.idx.poke(i)
        assertEquals(dut.cellOut.peek, cells(i), s"cell $i")
        assertEquals(dut.vout.peek, cells.updated(i, d"12'4095"), s"vout $i")
    }.withTier(tier).run()

  bothTiers("register file: conditional dynamic writes, dynamic reads"): tier =>
    (new RegFileDut).simulation { dut =>
      def dataOf(i: Int) = d"16'${i * 300 + 7}".bits
      dut.wren.poke(1)
      for i <- 0 until 8 do
        dut.waddr.poke(i)
        dut.wdata.poke(dataOf(i))
        simCtx.step()
      dut.wren.poke(0)
      for i <- 0 until 8 do
        dut.raddr.poke(i)
        assertEquals(dut.rdata.peek, dataOf(i), s"reg $i")
      // with wren low, writes must not go through
      dut.waddr.poke(3)
      dut.wdata.poke(h"FFFF")
      simCtx.step()
      dut.raddr.poke(3)
      assertEquals(dut.rdata.peek, dataOf(3))
    }.withTier(tier).run()

  bothTiers("byte-enable RAM: per-byte masked writes, dynamic read"): tier =>
    (new ByteMemDut).simulation { dut =>
      // full-word write at addr 2
      dut.waddr.poke(2)
      dut.wdata.poke(h"AABBCCDD")
      dut.wsel.poke(h"F")
      simCtx.step()
      // partial write at addr 2: only bytes 0 and 2 enabled (bytes 1 and 3 keep AA/CC)
      dut.wdata.poke(h"11223344")
      dut.wsel.poke(h"5")
      simCtx.step()
      dut.wsel.poke(h"0") // stop writing
      dut.raddr.poke(2)
      assertEquals(dut.rdata.peek, h"AA22CC44") // byte3=AA byte2=22 byte1=CC byte0=44
      dut.raddr.poke(5)
      assertEquals(dut.rdata.peek, h"00000000") // untouched word stays at its zero init
    }.withTier(tier).run()
end CompositeSimSpec
