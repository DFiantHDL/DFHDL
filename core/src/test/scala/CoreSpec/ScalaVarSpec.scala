package CoreSpec
import dfhdl.*
import munit.*

// The permission list for a Scala `var` holding a DFHDL value (`ScalaVarPhase`).
//
// A Scala `var` is rebound during ELABORATION while a DFHDL variable is assigned at RUNTIME with
// `:=`; the two look alike and mean different things. A `var` is therefore admitted only where it
// cannot express something the elaboration cannot honour, with no relaxation flag. The positive
// controls matter as much as the rejections: a rule that stops firing shows up here as
// "No error found", and a rule that fires too widely shows up as a compile error in this file.
class ScalaVarSpec extends DFSpec:
  // ~~~ POSITIVE CONTROLS ~~~

  test("elaboration-time accumulation in an ED design body"):
    class Acc extends EDDesign:
      val x = Bits(8) <> IN
      val y = Bits(32) <> OUT
      // a Scala `for` over a Scala range desugars to `(0 until 3).foreach(i => ...)`, so the
      // accumulation itself sits inside a lambda: the access rule must see through it
      private var acc: Bits[Int] <> VAL = x
      for (i <- 0 until 3) acc = acc ++ x
      val word = acc
      y <> word
    val top = Acc()
    val errs = dfc.getErrors
    assert(errs.isEmpty, errs.mkString("\n"))

  test("a local `var` inside a plain elaboration-time method"):
    class LocalVar extends EDDesign:
      val x = Bits(8) <> IN
      val y = Bits(32) <> OUT
      // declared AND accessed inside the same `def`, so no scope boundary is crossed
      def widen4: Bits[Int] <> VAL =
        var acc: Bits[Int] <> VAL = x
        for (i <- 0 until 3) acc = acc ++ x
        acc
      y <> widen4
    val top = LocalVar()
    val errs = dfc.getErrors
    assert(errs.isEmpty, errs.mkString("\n"))

  // ~~~ RULE 2: no declaration inside a sequential scope ~~~

  private val dclErr =
    """|A Scala `var` holding a DFHDL value cannot be declared inside a sequential scope.
       |A sequential scope (a process, an `initial` block, a method body, or a
       |register-transfer (RT) or dataflow (DF) design or domain body) is elaborated once,
       |not once per execution, so a Scala `var` there cannot accumulate across it:
       |reassigning it only rebinds the Scala name.
       |To Fix:
       |* To accumulate in hardware, declare a DFHDL variable (`<> VAR`) and assign it with `:=`.
       |* To accumulate during elaboration, move `acc` to an event-driven (ED) design or domain body.
       |* If `acc` is never reassigned, change it to a `val`.""".stripMargin

  test("declaration inside a process is rejected"):
    assertPluginError(dclErr)(
      """
      class Foo extends EDDesign:
        val x = Bits(8) <> IN
        val y = Bits(32) <> OUT
        process(all):
          var acc: Bits[Int] <> VAL = x
          for (i <- 0 until 3) acc = acc ++ x
          y := acc
      """
    )

  test("declaration inside an `initial` block is rejected"):
    assertPluginError(dclErr)(
      """
      class Foo extends EDDesign:
        val x = Bits(8) <> IN
        val y = Bits(8) <> OUT
        initial:
          var acc: Bits[8] <> VAL = x
          y := acc
      """
    )

  test("declaration in an RT design body is rejected"):
    assertPluginError(dclErr)(
      """
      class Foo extends RTDesign:
        val x = Bits(8) <> IN
        val y = Bits(8) <> OUT
        private var acc: Bits[8] <> VAL = x
        y := acc
      """
    )

  // ~~~ RULE 3: no access from a sequential scope, nor from inside a named method ~~~

  private def accessErr(why: String) =
    s"""|A Scala `var` holding a DFHDL value cannot be accessed from here.
        |$why
        |To Fix:
        |* Freeze the accumulated value first (`val frozen = acc`) and use that instead.
        |* To accumulate in hardware, declare a DFHDL variable (`<> VAR`) and assign it with `:=`.""".stripMargin

  private val seqAccessErr = accessErr(
    """|A sequential scope is elaborated once, so the access would run inside the
       |elaborated hardware rather than during the accumulation.""".stripMargin
  )

  private val methodAccessErr = accessErr(
    """|A method can be invoked from anywhere, including from inside a hardware loop, so
       |an access from a method body cannot be shown to run during elaboration.""".stripMargin
  )

  test("access from a process is rejected"):
    assertPluginError(seqAccessErr)(
      """
      class Foo extends EDDesign:
        val x = Bits(8) <> IN
        val y = Bits(8) <> OUT
        private var acc: Bits[8] <> VAL = x
        process(all):
          y := acc
      """
    )

  // the laundering hole a purely lexical declaration rule would leave open: the reassignment
  // is written at class scope, but it EXECUTES wherever the method is called from
  test("reassignment laundered through a named `def` is rejected"):
    assertPluginError(methodAccessErr)(
      """
      class Foo extends EDDesign:
        val x = Bits(8) <> IN
        val y = Bits(8) <> OUT
        private var acc: Bits[8] <> VAL = x
        def bump(): Unit = acc = acc | x
        bump()
        y <> acc
      """
    )

  // an HDL method body carries its own `Scope.Function`, so it is caught as a sequential scope
  // before the enclosing `def` is even reached
  test("a read from an ED method body is rejected"):
    assertPluginError(seqAccessErr)(
      """
      class Foo extends EDDesign:
        val x = Bits(8) <> IN
        val y = Bits(8) <> OUT
        private var acc: Bits[8] <> VAL = x
        def masked(): Bits[8] <> EDRET = acc | x
        y <> masked()
      """
    )

  // ~~~ RULE 4: `private` or local ~~~

  test("a public `var` member is rejected"):
    assertPluginError(
      """|A Scala `var` holding a DFHDL value must be `private`.
         |A public (or `protected`) `var` member stays reassignable from outside the design
         |once elaboration is over, and it takes part in the design's selectable surface.
         |To Fix: add the `private` modifier, or make `acc` a local `var`.""".stripMargin
    )(
      """
      class Foo extends EDDesign:
        val x = Bits(8) <> IN
        val y = Bits(8) <> OUT
        var acc: Bits[8] <> VAL = x
        y <> acc
      """
    )

  // ~~~ RULE 5: ascribed `<> VAL` or `<> CONST` ~~~

  test("an inferred type is rejected"):
    assertPluginError(
      """|A Scala `var` holding a DFHDL value must be explicitly ascribed `T <> VAL` or `T <> CONST`.
         |An inferred type comes from the initializer, so it fixes the width at the first
         |assignment (`var acc = x(0).bits` infers `Bits[1]`) and it carries the initializer's
         |scope, domain and assignability markers into every later use.
         |To Fix: write the type, e.g. `var acc: Bits[8] <> VAL = ...`.
         |Note that an unbounded `Int` width is checked at elaboration, not at compile time.""".stripMargin
    )(
      """
      class Foo extends EDDesign:
        val x = Bits(8) <> IN
        val y = Bits(8) <> OUT
        private var acc = x
        y <> acc
      """
    )

  test("an inferred CONSTANT type is rejected, and the suggestion keeps `<> CONST`"):
    assertPluginError(
      """|A Scala `var` holding a DFHDL value must be explicitly ascribed `T <> VAL` or `T <> CONST`.
         |An inferred type comes from the initializer, so it fixes the width at the first
         |assignment (`var acc = x(0).bits` infers `Bits[1]`) and it carries the initializer's
         |scope, domain and assignability markers into every later use.
         |To Fix: write the type, e.g. `var acc: UInt[8] <> CONST = ...`.
         |Note that an unbounded `Int` width is checked at elaboration, not at compile time.""".stripMargin
    )(
      """
      class Foo extends EDDesign:
        val y = UInt(8) <> OUT
        private var acc = d"8'0"
        y <> acc
      """
    )

  // an assignable ascription is what would let a Scala `var` rebind (`=`) and a DFHDL variable
  // assign (`:=`) the same name
  test("an assignable ascription is rejected"):
    assertPluginError(
      """|A Scala `var` holding a DFHDL value must be ascribed `T <> VAL` or `T <> CONST`, not `Bits[8] <> VAR`.
         |A Scala `var` is rebound with `=` during elaboration, while a DFHDL variable or port
         |is assigned with `:=` or connected with `<>`. Holding one in a Scala `var` mixes the two.
         |To Fix:
         |* To rebind a value during elaboration, ascribe `<> VAL` (or `<> CONST`).
         |* To assign in hardware, declare the variable in a `val` and use `:=`.""".stripMargin
    )(
      """
      class Foo extends EDDesign:
        val x = Bits(8) <> IN
        val y = Bits(8) <> OUT
        private var acc: Bits[8] <> OUT = y
        y <> x
      """
    )

  // ~~~ RULE 6: no design/domain/interface instance ~~~

  test("a `var` holding a design instance is rejected"):
    assertPluginError(
      """|A Scala `var` cannot hold a DFHDL design, domain, or interface instance.
         |An instance is structural: it is created once, during elaboration, and rebinding the
         |Scala name neither removes the old instance nor creates a new one.
         |To Fix: change the `var` to a `val`.""".stripMargin
    )(
      """
      class Inner extends EDDesign:
        val x = Bits(8) <> IN
        val y = Bits(8) <> OUT
        y <> x
      class Foo extends EDDesign:
        val x = Bits(8) <> IN
        val y = Bits(8) <> OUT
        private var inst: Inner = Inner()
        inst.x <> x
        y <> inst.y
      """
    )
end ScalaVarSpec
