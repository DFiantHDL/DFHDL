package dfhdl.sim

import dfhdl.internals.*
import scala.collection.mutable

/** A lowered value handle: the value's packed bits split into 64-bit lanes, LSB-first — `lanes(0)`
  * holds bits 63..0, `lanes(1)` holds bits 127..64, and the top lane holds the remaining width.
  * Lane node widths always follow [[WideOps.laneWidths]], so any two handles of the same width are
  * lane-compatible.
  */
private[sim] final case class WV(lanes: Vector[Int], width: Int) derives CanEqual

/** Word-slicing layer over the (64-bit-scalar) [[Netlist]]: every operation on arbitrarily wide
  * values is decomposed here into lane-wise scalar ops — carry/borrow chains for arithmetic,
  * lexicographic compare chains, lane-recombining constant shifts, barrel networks for dynamic
  * shifts, and OR-composition for concat/insert/extract. Single-lane (<= 64-bit) values degenerate
  * to the direct scalar ops, so narrow designs lower exactly as before.
  */
private[sim] final class WideOps(val nl: Netlist):
  def laneWidths(width: Int): Vector[Int] =
    require(width >= 1, s"unsupported width $width")
    Vector.tabulate((width + 63) / 64)(i => math.min(64, width - i * 64))

  private def laneLong(bits: BitVector, i: Int, lw: Int): Long =
    bits.bitsWL(lw, 64 * i).toLong(signed = false)

  // shared zero/all-ones constant lanes (referenced all over the decompositions)
  private val zeroLanes = mutable.Map.empty[Int, Int]
  private val onesLanes = mutable.Map.empty[Int, Int]
  private def zeroLane(w: Int): Int = zeroLanes.getOrElseUpdate(w, nl.const(w, 0L))
  private def onesLane(w: Int): Int = onesLanes.getOrElseUpdate(w, nl.const(w, -1L))
  def zero(width: Int): WV = WV(laneWidths(width).map(zeroLane), width)
  def ones(width: Int): WV = WV(laneWidths(width).map(onesLane), width)

  // ---- sources / state --------------------------------------------------------------------

  def const(width: Int, bits: BitVector): WV =
    require(bits.width == width, "constant bits/width mismatch")
    WV(laneWidths(width).zipWithIndex.map((lw, i) => nl.const(lw, laneLong(bits, i, lw))), width)

  def reg(width: Int, init: BitVector): WV =
    require(init.width == width, "register init/width mismatch")
    WV(laneWidths(width).zipWithIndex.map((lw, i) => nl.reg(lw, laneLong(init, i, lw))), width)

  def setNext(r: WV, n: WV): Unit =
    require(r.width == n.width, "width mismatch on register next")
    r.lanes.lazyZip(n.lanes).foreach(nl.setNext)

  def mov(width: Int): WV = WV(laneWidths(width).map(nl.mov), width)

  def patchMov(dst: WV, src: WV): Unit =
    require(dst.width == src.width, "width mismatch on MOV patch")
    dst.lanes.lazyZip(src.lanes).foreach(nl.patchMov)

  /** Per-lane ROM read of a constant table (any length — padded to a power of 2; out-of-range
    * addresses read zeros or wrap).
    */
  def rom(cells: Vector[BitVector], cellWidth: Int, addr: Int): WV =
    var pow2 = 1
    while pow2 < cells.length do pow2 <<= 1
    val padded = cells ++ Vector.fill(pow2 - cells.length)(BitVector.low(cellWidth))
    val lanes = laneWidths(cellWidth).zipWithIndex.map { (lw, i) =>
      nl.rom(padded.map(laneLong(_, i, lw)).toArray, lw, addr)
    }
    WV(lanes, cellWidth)

  // ---- bit plumbing -----------------------------------------------------------------------

  /** A single scalar node holding bits `lo..lo+w-1` of `v` (bits beyond `v.width` read 0). */
  def bitField(v: WV, lo: Int, w: Int): Int =
    require(w >= 1 && w <= 64 && lo >= 0, s"bad bit field lo=$lo w=$w")
    if lo >= v.width then zeroLane(w)
    else
      val k = lo / 64
      val r = lo % 64
      val lk = v.lanes(k)
      val loPart =
        if r == 0 then nl.resize(lk, w)
        else if r >= nl.widthOf(lk) then zeroLane(w)
        else nl.resize(nl.shr(lk, r), w)
      if r + w > 64 && k + 1 < v.lanes.length then
        nl.or(loPart, nl.shl(nl.resize(v.lanes(k + 1), w), 64 - r))
      else loPart

  /** Bits `lo..lo+w-1` of `v` as a value of width `w`. */
  def extract(v: WV, lo: Int, w: Int): WV =
    if lo == 0 && w == v.width then v
    else WV(laneWidths(w).zipWithIndex.map((lw, j) => bitField(v, lo + 64 * j, lw)), w)

  /** Compose non-overlapping `(value, bit offset)` parts into one value (uncovered bits are 0; part
    * bits beyond `width` are dropped).
    */
  def assemble(parts: Seq[(WV, Int)], width: Int): WV =
    val lanes = laneWidths(width).zipWithIndex.map { (lw, j) =>
      val base = 64 * j
      val contribs = parts.flatMap { (p, off) =>
        val s = math.max(off, base)
        val e = math.min(off + p.width, base + lw)
        if s >= e then None
        else
          val field = nl.resize(bitField(p, s - off, e - s), lw)
          Some(if s == base then field else nl.shl(field, s - base))
      }
      if contribs.isEmpty then zeroLane(lw) else contribs.reduce(nl.or)
    }
    WV(lanes, width)

  /** `base` with bits `lo..lo+part.width-1` replaced by `part`. */
  def insert(base: WV, part: WV, lo: Int): WV =
    require(lo >= 0 && lo + part.width <= base.width, "insert out of range")
    val parts = Seq.newBuilder[(WV, Int)]
    if lo > 0 then parts += extract(base, 0, lo) -> 0
    parts += part -> lo
    val hi = lo + part.width
    if hi < base.width then parts += extract(base, hi, base.width - hi) -> hi
    assemble(parts.result(), base.width)

  /** Concatenation, first part at the MSBs (DFHDL `++` / struct / vector composition order). */
  def concat(msbFirst: Seq[WV]): WV =
    val total = msbFirst.map(_.width).sum
    var off = total
    val parts = msbFirst.map { p =>
      off -= p.width
      p -> off
    }
    assemble(parts, total)

  def repeat(v: WV, count: Int): WV =
    assemble(Seq.tabulate(count)(k => v -> k * v.width), count * v.width)

  private def zextTo(v: WV, w: Int): WV =
    WV(
      laneWidths(w).zipWithIndex.map((lw, j) =>
        if j < v.lanes.length then nl.resize(v.lanes(j), lw) else zeroLane(lw)
      ),
      w
    )

  /** Width adjustment: truncate down; zero- or sign-extend up. */
  def resize(v: WV, w: Int, signed: Boolean): WV =
    if w == v.width then v
    else if w < v.width then extract(v, 0, w)
    else if !signed then zextTo(v, w)
    else
      val fill64 = nl.sub(zeroLane(64), nl.resize(bitField(v, v.width - 1, 1), 64))
      val k = v.lanes.length - 1 // old top lane
      val otw = v.width - 64 * k
      val lanes = laneWidths(w).zipWithIndex.map { (lw, j) =>
        if j < k then v.lanes(j)
        else if j == k then
          if otw == 64 then v.lanes(j)
          else nl.or(nl.resize(v.lanes(j), lw), nl.shl(nl.resize(fill64, lw), otw))
        else nl.resize(fill64, lw)
      }
      WV(lanes, w)

  /** All bits set to the sign bit of `v`. */
  private def signFill(v: WV): WV =
    resize(WV(Vector(bitField(v, v.width - 1, 1)), 1), v.width, signed = true)

  // ---- logic ------------------------------------------------------------------------------

  private def zipLanes(a: WV, b: WV)(op: (Int, Int) => Int): WV =
    require(a.width == b.width, "width mismatch")
    WV(a.lanes.lazyZip(b.lanes).map(op).toVector, a.width)

  def and(a: WV, b: WV): WV = zipLanes(a, b)(nl.and)
  def or(a: WV, b: WV): WV = zipLanes(a, b)(nl.or)
  def xor(a: WV, b: WV): WV = zipLanes(a, b)(nl.xor)
  def not(v: WV): WV = WV(v.lanes.map(nl.not), v.width)

  // ---- arithmetic -------------------------------------------------------------------------

  def add(a: WV, b: WV): WV =
    require(a.width == b.width, "width mismatch")
    val top = a.lanes.length - 1
    if top == 0 then WV(Vector(nl.add(a.lanes(0), b.lanes(0))), a.width)
    else
      val out = Vector.newBuilder[Int]
      var carry = -1
      for i <- 0 to top do
        val s1 = nl.add(a.lanes(i), b.lanes(i))
        if i == top then
          out +=
            (if carry == -1 then s1
             else nl.add(s1, nl.resize(carry, nl.widthOf(s1))))
        else if carry == -1 then
          out += s1
          carry = nl.ult(s1, a.lanes(i))
        else
          val s2 = nl.add(s1, nl.resize(carry, 64))
          out += s2
          carry = nl.or(nl.ult(s1, a.lanes(i)), nl.ult(s2, s1))
      end for
      WV(out.result(), a.width)
    end if
  end add

  def sub(a: WV, b: WV): WV =
    require(a.width == b.width, "width mismatch")
    val top = a.lanes.length - 1
    if top == 0 then WV(Vector(nl.sub(a.lanes(0), b.lanes(0))), a.width)
    else
      val out = Vector.newBuilder[Int]
      var borrow = -1
      for i <- 0 to top do
        val d1 = nl.sub(a.lanes(i), b.lanes(i))
        if i == top then
          out +=
            (if borrow == -1 then d1
             else nl.sub(d1, nl.resize(borrow, nl.widthOf(d1))))
        else if borrow == -1 then
          out += d1
          borrow = nl.ult(a.lanes(i), b.lanes(i))
        else
          val bin = nl.resize(borrow, 64)
          out += nl.sub(d1, bin)
          borrow = nl.or(nl.ult(a.lanes(i), b.lanes(i)), nl.ult(d1, bin))
      end for
      WV(out.result(), a.width)
    end if
  end sub

  def neg(v: WV): WV = sub(zero(v.width), v)

  // ---- comparisons (1-bit scalar node results) ---------------------------------------------

  def eqNode(a: WV, b: WV): Int =
    require(a.width == b.width, "width mismatch")
    a.lanes.lazyZip(b.lanes).map(nl.eq).reduce(nl.and)

  def neqNode(a: WV, b: WV): Int = nl.not(eqNode(a, b))

  /** Lexicographic less-than over the lanes; the top lane compares signed when requested. */
  def ltNode(a: WV, b: WV, signed: Boolean): Int =
    require(a.width == b.width, "width mismatch")
    val top = a.lanes.length - 1
    def laneLt(i: Int): Int =
      if i == top && signed then nl.slt(a.lanes(i), b.lanes(i))
      else nl.ult(a.lanes(i), b.lanes(i))
    var acc = laneLt(0)
    for i <- 1 to top do acc = nl.mux(nl.eq(a.lanes(i), b.lanes(i)), acc, laneLt(i))
    acc

  def mux(sel: Int, t: WV, f: WV): WV = zipLanes(t, f)(nl.mux(sel, _, _))

  // ---- shifts / rotates -------------------------------------------------------------------

  def shlConst(v: WV, n: Int): WV =
    require(n >= 0, "negative shift")
    if n == 0 then v
    else if n >= v.width then zero(v.width)
    else if v.lanes.sizeIs == 1 then WV(Vector(nl.shl(v.lanes(0), n)), v.width)
    else assemble(Seq(v -> n), v.width)

  def shrConst(v: WV, n: Int, arith: Boolean = false): WV =
    require(n >= 0, "negative shift")
    if n == 0 then v
    else if n >= v.width then if arith then signFill(v) else zero(v.width)
    else if v.lanes.sizeIs == 1 && !arith then WV(Vector(nl.shr(v.lanes(0), n)), v.width)
    else resize(extract(v, n, v.width - n), v.width, signed = arith)

  def rotr(v: WV, nRaw: Int): WV =
    val n = ((nRaw % v.width) + v.width) % v.width
    if n == 0 then v
    else if v.lanes.sizeIs == 1 then WV(Vector(nl.rotr(v.lanes(0), n)), v.width)
    else or(shrConst(v, n), shlConst(v, v.width - n))

  def rotl(v: WV, n: Int): WV = rotr(v, v.width - ((n % v.width) + v.width) % v.width)

  def shlDyn(v: WV, amt: WV): WV = shiftDyn(v, amt, left = true, arith = false)
  def shrDyn(v: WV, amt: WV, arith: Boolean = false): WV =
    shiftDyn(v, amt, left = false, arith)

  /** Barrel network for dynamic shift amounts; amount bits at or above the value width collapse
    * into a single fill mux.
    */
  private def shiftDyn(v: WV, amt: WV, left: Boolean, arith: Boolean): WV =
    if v.lanes.sizeIs == 1 && amt.lanes.sizeIs == 1 then
      val n =
        if left then nl.shlv(v.lanes(0), amt.lanes(0))
        else if arith then nl.srav(v.lanes(0), amt.lanes(0))
        else nl.shrv(v.lanes(0), amt.lanes(0))
      WV(Vector(n), v.width)
    else
      var x = v
      val overBits = List.newBuilder[Int]
      for k <- 0 until amt.width do
        val bit = bitField(amt, k, 1)
        if k >= 31 || (1L << k) >= v.width then overBits += bit
        else
          val shifted = if left then shlConst(x, 1 << k) else shrConst(x, 1 << k, arith)
          x = mux(bit, shifted, x)
      val over = overBits.result()
      if over.nonEmpty then
        val fill = if arith && !left then signFill(v) else zero(v.width)
        x = mux(over.reduce(nl.or), fill, x)
      x

  // ---- dynamic bit offsets (vector indexing) ------------------------------------------------

  /** Bits `off..off+w-1` of `v` where `off` is a dynamic value. */
  def dynExtract(v: WV, off: WV, w: Int): WV = extract(shrDyn(v, off), 0, w)

  /** `base` with bits `off..off+part.width-1` replaced by `part` (dynamic offset RMW). */
  def dynInsert(base: WV, part: WV, off: WV): WV =
    val mask = shlDyn(zextTo(ones(part.width), base.width), off)
    val shifted = shlDyn(zextTo(part, base.width), off)
    or(and(base, not(mask)), shifted)

  // ---- misc -------------------------------------------------------------------------------

  def reverse(v: WV): WV =
    if v.lanes.sizeIs == 1 then WV(Vector(nl.rev(v.lanes(0), v.width)), v.width)
    else
      // reverse each lane in full 64-bit space and swap the lane order: this reverses the
      // zero-padded value, so the true reversal sits at the top — shift it back down
      val l64 = v.lanes.length * 64
      val padded = WV(v.lanes.map(nl.rev(_, 64)).reverse, l64)
      extract(padded, l64 - v.width, v.width)
end WideOps
