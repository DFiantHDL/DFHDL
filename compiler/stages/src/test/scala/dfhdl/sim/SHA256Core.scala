package dfhdl.sim
import dfhdl.*

/** SHA-256 round constants ROM */
val shaK: Bits[32] X 64 <> CONST = Vector(
  h"428a2f98",
  h"71374491",
  h"b5c0fbcf",
  h"e9b5dba5",
  h"3956c25b",
  h"59f111f1",
  h"923f82a4",
  h"ab1c5ed5",
  h"d807aa98",
  h"12835b01",
  h"243185be",
  h"550c7dc3",
  h"72be5d74",
  h"80deb1fe",
  h"9bdc06a7",
  h"c19bf174",
  h"e49b69c1",
  h"efbe4786",
  h"0fc19dc6",
  h"240ca1cc",
  h"2de92c6f",
  h"4a7484aa",
  h"5cb0a9dc",
  h"76f988da",
  h"983e5152",
  h"a831c66d",
  h"b00327c8",
  h"bf597fc7",
  h"c6e00bf3",
  h"d5a79147",
  h"06ca6351",
  h"14292967",
  h"27b70a85",
  h"2e1b2138",
  h"4d2c6dfc",
  h"53380d13",
  h"650a7354",
  h"766a0abb",
  h"81c2c92e",
  h"92722c85",
  h"a2bfe8a1",
  h"a81a664b",
  h"c24b8b70",
  h"c76c51a3",
  h"d192e819",
  h"d6990624",
  h"f40e3585",
  h"106aa070",
  h"19a4c116",
  h"1e376c08",
  h"2748774c",
  h"34b0bcb5",
  h"391c0cb3",
  h"4ed8aa4a",
  h"5b9cca4f",
  h"682e6ff3",
  h"748f82ee",
  h"78a5636f",
  h"84c87814",
  h"8cc70208",
  h"90befffa",
  h"a4506ceb",
  h"bef9a3f7",
  h"c67178f2"
)

/** SHA-256 compression core, one round per cycle. The DFHDL twin of the DFacsimile kernel mockup
  * benchmark (secworks/sha256-style). The working state (a..h), the 16-word message schedule window
  * (preloaded with the padded "abc" block), and the round counter are registers; after 64 cycles
  * `IV + state` is the digest of "abc".
  */
class SHA256Core extends RTDesign:
  val sink = Bits(32) <> OUT

  val a = Bits(32) <> VAR.REG init h"6a09e667"
  val b = Bits(32) <> VAR.REG init h"bb67ae85"
  val c = Bits(32) <> VAR.REG init h"3c6ef372"
  val d = Bits(32) <> VAR.REG init h"a54ff53a"
  val e = Bits(32) <> VAR.REG init h"510e527f"
  val f = Bits(32) <> VAR.REG init h"9b05688c"
  val g = Bits(32) <> VAR.REG init h"1f83d9ab"
  val h = Bits(32) <> VAR.REG init h"5be0cd19"

  val w15 = Bits(32) <> VAR.REG init h"00000018"
  val w14 = w15.reg(1, init = h"00000000")
  val w9 = w14.reg(5, init = h"00000000")
  val w1 = w9.reg(8, init = h"00000000")
  val w0 = w1.reg(1, init = h"61626380")

  val t = UInt(6) <> VAR.REG init 0

  val kk = shaK(t)
  val s1 =
    (e >> 6 | e << 26) ^
      (e >> 11 | e << 21) ^
      (e >> 25 | e << 7)
  val ch = (e & f) ^ (~e & g)
  val t1 = (h.uint + s1.uint + ch.uint + kk.uint + w0.uint).bits
  val s0 =
    (a >> 2 | a << 30) ^
      (a >> 13 | a << 19) ^
      (a >> 22 | a << 10)
  val maj = (a & b) ^ (a & c) ^ (b & c)

  h.din := g
  g.din := f
  f.din := e
  e.din := (d.uint + t1.uint).bits
  d.din := c
  c.din := b
  b.din := a
  a.din := (t1.uint + s0.uint + maj.uint).bits

  val sw0 = (w1 >> 7 | w1 << 25) ^ (w1 >> 18 | w1 << 14) ^ (w1 >> 3)
  val sw1 = (w14 >> 17 | w14 << 15) ^ (w14 >> 19 | w14 << 13) ^ (w14 >> 10)
  val nw = (sw1.uint + w9.uint + sw0.uint + w0.uint).bits

  w15.din := nw

  t.din := t + 1

  sink := a ^ e
end SHA256Core
