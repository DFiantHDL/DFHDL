package dfhdl

/** Target-context widening of anonymous arithmetic (issue dfhdl_by_agents#119): an anonymous `+`,
  * `-`, `*` cone assigned or connected to a wider value evaluates at the target's width and sign,
  * matching Verilog's assignment-context width propagation. A binary op over leaf operands whose
  * carry form fits the target elaborates as a carry op (never extended, only truncated); any other
  * widening converts the operands, printed relatively as `.eby(k)` when the target width is a
  * literal increment of the source width.
  *
  * The parametric shapes are the ones a literal-width core spec cannot host (see
  * `CoreSpec.DFDecimalSpec` for the literal-width matrix): symbolic carry-fit decisions (`W + 1`
  * fits `+^`; `W + W` fits `*^`) and the relative `.eby(k)` printing.
  *
  * The width-fit acceptance itself is proof-backed (issue dfhdl_by_agents#116): a parametric
  * relation such as `2 * W >= W` is accepted because it holds for every valid (positive) width
  * assignment, while a relation that a valid assignment can violate (e.g. `16 >= W`) still requires
  * an explicit carry op or `.resize` (see `ElaborationChecksSpec` for the rejections).
  */
class ContextWidenSpec extends DesignSpec:
  test("parametric target-context widening") {
    @top(false) class ParamWiden(val W: Int <> CONST = 8) extends EDDesign:
      val a, b = SInt(W) <> IN
      val ua, ub = UInt(W) <> IN
      val sum = SInt(W + 1) <> OUT
      val usub = UInt(W + 1) <> OUT
      val acc = SInt(W + 2) <> OUT
      val chain = SInt(W + 2) <> OUT
      // carry fit is decided symbolically: (W max W) + 1 =~ W + 1
      sum <> a + b
      // an unsigned subtraction carry-fits only EXACTLY (its carry result is a wrap
      // pattern, which must never be extended)
      usub <> ua - ub
      // beyond the carry width: operands widen to the target width and the op stays
      // modular there, exactly like Verilog's assignment context
      acc <> a + b
      // a merged (non-binary) chain always evaluates at the target width; the implicit
      // Int operand adapts at the operand width and widens along
      chain <> a + b + 1
    end ParamWiden

    ParamWiden().assertCodeString(
      """|class ParamWiden(val W: Int <> CONST = 8) extends EDDesign:
         |  val a = SInt(W) <> IN
         |  val b = SInt(W) <> IN
         |  val ua = UInt(W) <> IN
         |  val ub = UInt(W) <> IN
         |  val sum = SInt(W + 1) <> OUT
         |  val usub = UInt(W + 1) <> OUT
         |  val acc = SInt(W + 2) <> OUT
         |  val chain = SInt(W + 2) <> OUT
         |  sum <> (a +^ b)
         |  usub <> (ua -^ ub)
         |  acc <> (a.eby(2) + b.eby(2))
         |  chain <> (a.eby(2) + b.eby(2) + sd"2'1".resize(W).eby(2))
         |end ParamWiden
         |""".stripMargin
    )
  }

  test("parametric mul target-context widening (width-fit proofs)") {
    @top(false) class ParamMul(val W: Int <> CONST = 8) extends EDDesign:
      val a, b = SInt(W) <> IN
      val ua, ub = UInt(W) <> IN
      val prod = SInt(2 * W) <> OUT
      val prod2 = SInt(W + W) <> OUT
      val uprod = UInt(2 * W) <> OUT
      val named = SInt(2 * W) <> OUT
      // the width-fit check accepts by proof: 2 * W >= W for every valid (positive) W
      prod <> a * b
      // ref-shape independence: the carry fit decides symbolically, W + W =~ 2 * W
      prod2 <> a * b
      uprod <> ua * ub
      // a NAMED product evaluates at the operand width and resizes to the target, printed
      // absolutely (the width delta is symbolic, so no relative `.eby` spelling)
      val p = a * b
      named <> p
    end ParamMul

    ParamMul().assertCodeString(
      """|class ParamMul(val W: Int <> CONST = 8) extends EDDesign:
         |  val a = SInt(W) <> IN
         |  val b = SInt(W) <> IN
         |  val ua = UInt(W) <> IN
         |  val ub = UInt(W) <> IN
         |  val prod = SInt(2 * W) <> OUT
         |  val prod2 = SInt(W + W) <> OUT
         |  val uprod = UInt(2 * W) <> OUT
         |  val named = SInt(2 * W) <> OUT
         |  prod <> (a *^ b)
         |  prod2 <> (a *^ b)
         |  uprod <> (ua *^ ub)
         |  val p = a * b
         |  named <> p.resize(2 * W)
         |end ParamMul
         |""".stripMargin
    )
  }

  test("explicit eby") {
    @top(false) class Eby(val W: Int <> CONST = 8) extends EDDesign:
      val a = SInt(W) <> IN
      val ua = UInt(W) <> IN
      val b = Bits(W) <> IN
      val ax = SInt(W + 3) <> OUT
      val ux = UInt(W + 1) <> OUT
      val bx = Bits(W + 2) <> OUT
      ax <> a.eby(3)
      ux <> ua.eby(1)
      bx <> b.eby(2)

    Eby().assertCodeString(
      """|class Eby(val W: Int <> CONST = 8) extends EDDesign:
         |  val a = SInt(W) <> IN
         |  val ua = UInt(W) <> IN
         |  val b = Bits(W) <> IN
         |  val ax = SInt(W + 3) <> OUT
         |  val ux = UInt(W + 1) <> OUT
         |  val bx = Bits(W + 2) <> OUT
         |  ax <> a.eby(3)
         |  ux <> ua.eby(1)
         |  bx <> b.eby(2)
         |end Eby
         |""".stripMargin
    )
  }
end ContextWidenSpec
