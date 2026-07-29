package StagesSpec

import dfhdl.*
// scalafmt: { align.tokens = [{code = "<>"}, {code = "="}, {code = "=>"}, {code = ":="}]}

/** Tests for CLASS designs routed through the design load gate: designs unify ONLY through the
  * gate's key (the structural `=~` dedup is retired). A class design's key covers its declaration
  * meta, its plain Scala constructor parameters and template captures (`__clsScalaArgs`), and the
  * applied data of its data-impure `<> CONST` parameters (named on the `pure` annotation by the
  * PureCheck phase).
  */
class ClassDesignKeySpec extends StageSpec:
  test("identical class instantiations unify; applied const params stay parametric") {
    class Child(val width: Int <> CONST) extends DFDesign:
      val x = UInt(width) <> IN
      val y = UInt(width) <> OUT
      y := x
    class Top extends DFDesign:
      val x  = UInt(8) <> IN
      val y  = UInt(8) <> OUT
      val c1 = new Child(8)
      val c2 = new Child(16)
      c1.x <> x
      c2.x <> c1.y.resize(16)
      y    <> c2.y.resize(8)
    end Top
    // the applied parameter values are NOT part of the key (a pure body cannot depend
    // on them), so both instances share one parametric design with per-instance
    // parameter bindings
    assertCodeString(
      new Top,
      """|class Child(val width: Int <> CONST) extends DFDesign:
         |  val x = UInt(width) <> IN
         |  val y = UInt(width) <> OUT
         |  y := x
         |end Child
         |
         |class Top extends DFDesign:
         |  val x = UInt(8) <> IN
         |  val y = UInt(8) <> OUT
         |  val c1 = Child(width = 8)
         |  val c2 = Child(width = 16)
         |  c1.x <> x
         |  c2.x <> c1.y.resize(16)
         |  y <> c2.y.resize(8)
         |end Top""".stripMargin
    )
  }

  test("plain Scala constructor arguments join the class key") {
    class Repeater(n: Int) extends DFDesign:
      val x = UInt(8) <> IN
      val y = UInt(8) <> OUT
      y := x + n
    class Top extends DFDesign:
      val x  = UInt(8) <> IN
      val y  = UInt(8) <> OUT
      val r1 = new Repeater(1)
      val r2 = new Repeater(1)
      val r3 = new Repeater(2)
      r1.x <> x
      r2.x <> r1.y
      r3.x <> r2.y
      y    <> r3.y
    end Top
    // a Scala argument may legitimately shape the elaborated structure, so it is part
    // of the key: same value unifies, different value splits
    assertCodeString(
      new Top,
      """|class Repeater_0 extends DFDesign:
         |  val x = UInt(8) <> IN
         |  val y = UInt(8) <> OUT
         |  y := x + d"8'1"
         |end Repeater_0
         |
         |class Repeater_1 extends DFDesign:
         |  val x = UInt(8) <> IN
         |  val y = UInt(8) <> OUT
         |  y := x + d"8'2"
         |end Repeater_1
         |
         |class Top extends DFDesign:
         |  val x = UInt(8) <> IN
         |  val y = UInt(8) <> OUT
         |  val r1 = Repeater_0()
         |  val r2 = Repeater_0()
         |  val r3 = Repeater_1()
         |  r1.x <> x
         |  r2.x <> r1.y
         |  r3.x <> r2.y
         |  y <> r3.y
         |end Top""".stripMargin
    )
  }

  test("plain Scala template captures join the class key") {
    // NOTE: a List-lambda rather than a `for` comprehension over a range: at design
    // level a range `for`/`foreach` belongs to the plugin's DFHDL-loop transformation,
    // and instantiating a design inside one is a pre-existing limitation unrelated to
    // the design load gate
    class Top extends DFDesign:
      val x = UInt(8) <> IN
      val y = UInt(8) <> OUT
      y := List(0, 1).foldLeft[UInt[8] <> VAL](x) { (acc, i) =>
        class Adder extends DFDesign:
          val a = UInt(8) <> IN
          val b = UInt(8) <> OUT
          b := a + i
        val adder = new Adder
        adder.a <> acc
        adder.b
      }
    end Top
    // the same LOCAL class captures a different loop value per instantiation: the
    // capture is part of the key (`__clsScalaArgs`), so the two instances do not
    // wrongly unify. (Both instance vals print as `adder`: same-named design instances
    // from disjoint Scala scopes are not enumerated by the printer.)
    assertCodeString(
      new Top,
      """|class Adder_0 extends DFDesign:
         |  val a = UInt(8) <> IN
         |  val b = UInt(8) <> OUT
         |  b := a + d"8'0"
         |end Adder_0
         |
         |class Adder_1 extends DFDesign:
         |  val a = UInt(8) <> IN
         |  val b = UInt(8) <> OUT
         |  b := a + d"8'1"
         |end Adder_1
         |
         |class Top extends DFDesign:
         |  val x = UInt(8) <> IN
         |  val y = UInt(8) <> OUT
         |  val adder = Adder_0()
         |  adder.a <> x
         |  val adder = Adder_1()
         |  adder.a <> adder.b
         |  y := adder.b
         |end Top""".stripMargin
    )
  }

  test("toScalaXYZ forcing rooted at a class param keys the param's applied data") {
    class Folder(val amount: UInt[8] <> CONST) extends DFDesign:
      val x = UInt(32) <> IN
      val y = UInt(32) <> OUT
      y := x + amount.toScalaInt
    class Top extends DFDesign:
      val x  = UInt(32) <> IN
      val y  = UInt(32) <> OUT
      val f1 = new Folder(1)
      val f2 = new Folder(10)
      val f3 = new Folder(1)
      f1.x <> x
      f2.x <> f1.y
      f3.x <> f2.y
      y    <> f3.y
    end Top
    // the forced data derives from the class param `amount`, so only that PARAM is
    // marked data-impure (recorded by name on the CLASS annotation) and the class stays
    // pure and keyable: different applied values elaborate separate designs with their
    // folded constants, while a repeated value unifies (f3 joins f1's design)
    assertCodeString(
      new Top,
      """|@hw.annotation.pure(impureParams = "amount")
         |class Folder_0(val amount: UInt[8] <> CONST) extends DFDesign:
         |  val x = UInt(32) <> IN
         |  val y = UInt(32) <> OUT
         |  y := x + d"32'1"
         |end Folder_0
         |
         |@hw.annotation.pure(impureParams = "amount")
         |class Folder_1(val amount: UInt[8] <> CONST) extends DFDesign:
         |  val x = UInt(32) <> IN
         |  val y = UInt(32) <> OUT
         |  y := x + d"32'10"
         |end Folder_1
         |
         |class Top extends DFDesign:
         |  val x = UInt(32) <> IN
         |  val y = UInt(32) <> OUT
         |  val f1 = Folder_0(amount = d"8'1")
         |  val f2 = Folder_1(amount = d"8'10")
         |  val f3 = Folder_0(amount = d"8'1")
         |  f1.x <> x
         |  f2.x <> f1.y
         |  f3.x <> f2.y
         |  y <> f3.y
         |end Top""".stripMargin
    )
  }

  test("const Boolean guard inlining rooted at a class param keys the param's applied data") {
    class Cond(val arg: Boolean <> CONST) extends DFDesign:
      val x = UInt(32) <> IN
      val y = UInt(32) <> OUT
      if (arg) y := x + 1
      else y     := x + 2
    class Top extends DFDesign:
      val x  = UInt(32) <> IN
      val y  = UInt(32) <> OUT
      val c1 = new Cond(true)
      val c2 = new Cond(false)
      val c3 = new Cond(true)
      c1.x <> x
      c2.x <> c1.y
      c3.x <> c2.y
      y    <> c3.y
    end Top
    // the guard is constant and a design body has no conditional-statement capability, so
    // the Boolean conversion expands at typer into a Scala `if` that READS the param's
    // data (`toScalaBoolean`): the purity check sees that forcing and marks the param
    // data-impure exactly like an explicit toScalaXYZ call, so the applied value joins
    // the design key. Different applied values elaborate separate designs (with only the
    // taken branch), while a repeated value unifies (c3 joins c1's design).
    assertCodeString(
      new Top,
      """|@hw.annotation.pure(impureParams = "arg")
         |class Cond_0(val arg: Boolean <> CONST) extends DFDesign:
         |  val x = UInt(32) <> IN
         |  val y = UInt(32) <> OUT
         |  y := x + d"32'1"
         |end Cond_0
         |
         |@hw.annotation.pure(impureParams = "arg")
         |class Cond_1(val arg: Boolean <> CONST) extends DFDesign:
         |  val x = UInt(32) <> IN
         |  val y = UInt(32) <> OUT
         |  y := x + d"32'2"
         |end Cond_1
         |
         |class Top extends DFDesign:
         |  val x = UInt(32) <> IN
         |  val y = UInt(32) <> OUT
         |  val c1 = Cond_0(arg = true)
         |  val c2 = Cond_1(arg = false)
         |  val c3 = Cond_0(arg = true)
         |  c1.x <> x
         |  c2.x <> c1.y
         |  c3.x <> c2.y
         |  y <> c3.y
         |end Top""".stripMargin
    )
  }

  test("const Boolean guard inside a process stays a DFHDL conditional and keys nothing") {
    class Cond(val arg: Boolean <> CONST) extends EDDesign:
      val x = UInt(32) <> IN
      val y = UInt(32) <> OUT
      process(all):
        if (arg) y :== x + 1
        else y     :== x + 2
    class Top extends EDDesign:
      val x  = UInt(32) <> IN
      val y  = UInt(32) <> OUT
      val c1 = new Cond(true)
      val c2 = new Cond(false)
      c1.x <> x
      c2.x <> c1.y
      y    <> c2.y
    end Top
    // inside a process the scope HAS conditional-statement capability, so the constant
    // guard is NOT inlined: the `if` remains a DFHDL conditional over the parametric
    // `arg`, no data is forced (no impure-param marking), and both instances unify into
    // ONE parametric design despite their different applied values
    assertCodeString(
      new Top,
      """|class Cond(val arg: Boolean <> CONST) extends EDDesign:
         |  val x = UInt(32) <> IN
         |  val y = UInt(32) <> OUT
         |  process(all):
         |    if (arg) y :== x + d"32'1"
         |    else y :== x + d"32'2"
         |end Cond
         |
         |class Top extends EDDesign:
         |  val x = UInt(32) <> IN
         |  val y = UInt(32) <> OUT
         |  val c1 = Cond(arg = true)
         |  val c2 = Cond(arg = false)
         |  c1.x <> x
         |  c2.x <> c1.y
         |  y <> c2.y
         |end Top""".stripMargin
    )
  }
end ClassDesignKeySpec
