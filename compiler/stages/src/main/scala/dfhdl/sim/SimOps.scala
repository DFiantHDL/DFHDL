package dfhdl.sim

/** Pure single-op evaluation over `Long` lanes — the one place the kernel op semantics are defined
  * for build-time use. The netlist's constant folding evaluates through here, so a folded constant
  * and the interpreter's runtime result can only diverge if the interpreter itself diverges (which
  * the dual-tier test suite guards).
  *
  * `w` is the destination width, `aw` the first operand's width (sign-extension source for SLT).
  * `b` doubles as the immediate for the immediate-shift/rotate ops, matching the node encoding.
  */
private[sim] object SimOps:
  def maskFor(w: Int): Long = if w == 64 then -1L else (1L << w) - 1

  def eval(op: Int, w: Int, aw: Int, a: Long, b: Long, c: Long): Long =
    val m = maskFor(w)
    op match
      case Op.MOV    => a
      case Op.NOT    => ~a & m
      case Op.RESIZE => a & m
      case Op.REV    => java.lang.Long.reverse(a) >>> (64 - w)
      case Op.SHL    => (a << b) & m
      case Op.SHR    => a >>> b
      case Op.ROTR   => ((a >>> b) | (a << (w - b))) & m
      case Op.ADD    => (a + b) & m
      case Op.SUB    => (a - b) & m
      case Op.MUL    => (a * b) & m
      case Op.UDIV   => if b == 0L then 0L else java.lang.Long.divideUnsigned(a, b)
      case Op.SDIV   =>
        val s = 64 - w
        val bv = (b << s) >> s
        if bv == 0L then 0L else (((a << s) >> s) / bv) & m
      case Op.UREM => if b == 0L then 0L else java.lang.Long.remainderUnsigned(a, b)
      case Op.SREM =>
        val s = 64 - w
        val bv = (b << s) >> s
        if bv == 0L then 0L else (((a << s) >> s) % bv) & m
      case Op.AND => a & b
      case Op.OR  => a | b
      case Op.XOR => a ^ b
      case Op.EQ  => if a == b then 1L else 0L
      case Op.NEQ => if a != b then 1L else 0L
      case Op.ULT => if java.lang.Long.compareUnsigned(a, b) < 0 then 1L else 0L
      case Op.SLT =>
        val s = 64 - aw
        if ((a << s) >> s) < ((b << s) >> s) then 1L else 0L
      case Op.SHLV => if b >= 64L then 0L else (a << b) & m
      case Op.SHRV => if b >= 64L then 0L else a >>> b
      case Op.SRAV =>
        val s = 64 - w
        (((a << s) >> s) >> math.min(b, 63L)) & m
      case Op.MUX => if a != 0L then b else c
      case other  => throw new IllegalStateException(s"bad opcode $other")
    end match
  end eval
end SimOps
