package StagesSpec

import dfhdl.*
import dfhdl.compiler.stages.nameVarVersions
// scalafmt: { align.tokens = [{code = "<>"}, {code = "="}, {code = "=>"}, {code = ":="}]}

class NameVarVersionsSpec extends StageSpec:
  test("shared write reading a later-reassigned wire is captured") {
    class Example extends EDDesign:
      val ram = Bits(8) X 4 <> VAR.SHARED
      val a   = new RTDomain:
        val x    = Bits(8) <> IN
        val we   = Bit     <> IN
        val addr = Bits(2) <> IN
        val y    = Bits(8) <> OUT
        val v    = Bits(8) <> VAR
        v                 := x
        if (we) ram(addr) := v
        v                 := v | h"0f"
        y                 := v
    val top = (new Example).nameVarVersions
    assertCodeString(
      top,
      """class Example extends EDDesign:
        |  val ram = Bits(8) X 4 <> VAR.SHARED
        |  val a = new RTDomain:
        |    val x = Bits(8) <> IN
        |    val we = Bit <> IN
        |    val addr = Bits(2) <> IN
        |    val y = Bits(8) <> OUT
        |    val v = Bits(8) <> VAR
        |    val v_ver = Bits(8) <> VAR
        |    v_ver := h"??"
        |    v := x
        |    if (we)
        |      v_ver := v
        |      ram(addr.uint.toInt) := v_ver
        |    end if
        |    v := v | h"0f"
        |    y := v
        |  end a
        |end Example
        |""".stripMargin
    )
  }
  test("register write captures enumerate per site") {
    class Example extends EDDesign:
      val a = new RTDomain:
        val x  = Bits(8) <> IN
        val y  = Bits(8) <> OUT
        val q  = Bits(8) <> OUT.REG
        val r2 = Bits(8) <> OUT.REG
        val v  = Bits(8) <> VAR
        v      := x
        q.din  := v
        v      := v | h"01"
        r2.din := v
        v      := v | h"02"
        y      := v
    val top = (new Example).nameVarVersions
    assertCodeString(
      top,
      """class Example extends EDDesign:
        |  val a = new RTDomain:
        |    val x = Bits(8) <> IN
        |    val y = Bits(8) <> OUT
        |    val q = Bits(8) <> OUT.REG
        |    val r2 = Bits(8) <> OUT.REG
        |    val v = Bits(8) <> VAR
        |    val v_ver1 = Bits(8) <> VAR
        |    v_ver1 := h"??"
        |    val v_ver2 = Bits(8) <> VAR
        |    v_ver2 := h"??"
        |    v := x
        |    v_ver1 := v
        |    q.din := v_ver1
        |    v := v | h"01"
        |    v_ver2 := v
        |    r2.din := v_ver2
        |    v := v | h"02"
        |    y := v
        |  end a
        |end Example
        |""".stripMargin
    )
  }
  test("settled reads need no capture") {
    class Example extends EDDesign:
      val a = new RTDomain:
        val x = Bits(8) <> IN
        val y = Bits(8) <> OUT
        val q = Bits(8) <> OUT.REG
        val v = Bits(8) <> VAR
        v     := x
        v     := v | h"0f"
        q.din := v
        y     := v
    val top = (new Example).nameVarVersions
    assertCodeString(
      top,
      """class Example extends EDDesign:
        |  val a = new RTDomain:
        |    val x = Bits(8) <> IN
        |    val y = Bits(8) <> OUT
        |    val q = Bits(8) <> OUT.REG
        |    val v = Bits(8) <> VAR
        |    v := x
        |    v := v | h"0f"
        |    q.din := v
        |    y := v
        |  end a
        |end Example
        |""".stripMargin
    )
  }
  test("din-read register sites are skipped (shadow form)") {
    class Example extends EDDesign:
      val a = new RTDomain:
        val x = Bits(8) <> IN
        val y = Bits(8) <> OUT
        val q = Bits(8) <> OUT.REG
        val v = Bits(8) <> VAR
        v     := x
        q.din := v
        v     := v | h"0f"
        y     := v ^ q.din
    val top = (new Example).nameVarVersions
    assertCodeString(
      top,
      """class Example extends EDDesign:
        |  val a = new RTDomain:
        |    val x = Bits(8) <> IN
        |    val y = Bits(8) <> OUT
        |    val q = Bits(8) <> OUT.REG
        |    val v = Bits(8) <> VAR
        |    v := x
        |    q.din := v
        |    v := v | h"0f"
        |    y := v ^ q.din
        |  end a
        |end Example
        |""".stripMargin
    )
  }
  test("text output arguments are captured") {
    class Example extends EDDesign:
      val a = new RTDomain:
        val x = Bits(8) <> IN
        val y = Bits(8) <> OUT
        val v = Bits(8) <> VAR
        v := x
        println(v)
        v := v | h"0f"
        y := v
    val top = (new Example).nameVarVersions
    assertCodeString(
      top,
      """class Example extends EDDesign:
        |  val a = new RTDomain:
        |    val x = Bits(8) <> IN
        |    val y = Bits(8) <> OUT
        |    val v = Bits(8) <> VAR
        |    val v_ver = Bits(8) <> VAR
        |    v_ver := h"??"
        |    v := x
        |    v_ver := v
        |    println(s"${v_ver}")
        |    v := v | h"0f"
        |    y := v
        |  end a
        |end Example
        |""".stripMargin
    )
  }
  // a Scala `for` over a Scala range unrolls at elaboration, so each unrolled write is its own
  // capture site; an IR-level loop block site is skipped by the v1 loops-atomic rule
  test("unrolled Scala loops capture per site") {
    class Example extends EDDesign:
      val ram = Bits(8) X 4 <> VAR.SHARED
      val a   = new RTDomain:
        val x = Bits(8) <> IN
        val y = Bits(8) <> OUT
        val v = Bits(8) <> VAR
        v := x
        for (i <- 0 until 4)
          ram(i) := v
        v        := v | h"0f"
        y        := v
    val top = (new Example).nameVarVersions
    assertCodeString(
      top,
      """class Example extends EDDesign:
        |  val ram = Bits(8) X 4 <> VAR.SHARED
        |  val a = new RTDomain:
        |    val x = Bits(8) <> IN
        |    val y = Bits(8) <> OUT
        |    val v = Bits(8) <> VAR
        |    val v_ver1 = Bits(8) <> VAR
        |    v_ver1 := h"??"
        |    val v_ver2 = Bits(8) <> VAR
        |    v_ver2 := h"??"
        |    val v_ver3 = Bits(8) <> VAR
        |    v_ver3 := h"??"
        |    val v_ver4 = Bits(8) <> VAR
        |    v_ver4 := h"??"
        |    v := x
        |    v_ver1 := v
        |    ram(0) := v_ver1
        |    v_ver2 := v
        |    ram(1) := v_ver2
        |    v_ver3 := v
        |    ram(2) := v_ver3
        |    v_ver4 := v
        |    ram(3) := v_ver4
        |    v := v | h"0f"
        |    y := v
        |  end a
        |end Example
        |""".stripMargin
    )
  }
  test("captures inside nested conditionals") {
    class Example extends EDDesign:
      val ram = Bits(8) X 4 <> VAR.SHARED
      val a   = new RTDomain:
        val x    = Bits(8) <> IN
        val en   = Bit     <> IN
        val we   = Bit     <> IN
        val addr = Bits(2) <> IN
        val y    = Bits(8) <> OUT
        val v    = Bits(8) <> VAR
        v := x
        if (en)
          if (we) ram(addr) := v
        v := v | h"0f"
        y := v
    val top = (new Example).nameVarVersions
    assertCodeString(
      top,
      """class Example extends EDDesign:
        |  val ram = Bits(8) X 4 <> VAR.SHARED
        |  val a = new RTDomain:
        |    val x = Bits(8) <> IN
        |    val en = Bit <> IN
        |    val we = Bit <> IN
        |    val addr = Bits(2) <> IN
        |    val y = Bits(8) <> OUT
        |    val v = Bits(8) <> VAR
        |    val v_ver = Bits(8) <> VAR
        |    v_ver := h"??"
        |    v := x
        |    if (en)
        |      if (we)
        |        v_ver := v
        |        ram(addr.uint.toInt) := v_ver
        |      end if
        |    end if
        |    v := v | h"0f"
        |    y := v
        |  end a
        |end Example
        |""".stripMargin
    )
  }
  test("an unsettled chain guard skips a site in a later branch") {
    class Example extends EDDesign:
      val a = new RTDomain:
        val x  = Bits(8) <> IN
        val y  = Bit     <> OUT
        val y2 = Bits(8) <> OUT
        val q  = Bits(8) <> OUT.REG
        val c  = Bit     <> VAR
        val v  = Bits(8) <> VAR
        v         := x
        c         := x(0)
        y2        := h"00"
        if (c) y2 := x
        else if (x(1)) q.din := v
        v := v | h"0f"
        c := c || x(1)
        y := c
    end Example
    val top = (new Example).nameVarVersions
    assertCodeString(
      top,
      """class Example extends EDDesign:
        |  val a = new RTDomain:
        |    val x = Bits(8) <> IN
        |    val y = Bit <> OUT
        |    val y2 = Bits(8) <> OUT
        |    val q = Bits(8) <> OUT.REG
        |    val c = Bit <> VAR
        |    val v = Bits(8) <> VAR
        |    v := x
        |    c := x(0)
        |    val a: Bits[8] <> CONST = h"00"
        |    y2 := a
        |    if (c) y2 := x
        |    else if (x(1)) q.din := v
        |    v := v | h"0f"
        |    c := c || x(1)
        |    y := c
        |  end a
        |end Example
        |""".stripMargin
    )
  }
  test("report and assert arguments are captured") {
    class Example extends EDDesign:
      val a = new RTDomain:
        val x = Bits(8) <> IN
        val y = Bits(8) <> OUT
        val v = Bits(8) <> VAR
        v := x
        report(s"v is ${v}", Severity.Warning)
        assert(v == h"05", s"bad ${v}")
        v := v | h"0f"
        y := v
    val top = (new Example).nameVarVersions
    assertCodeString(
      top,
      """class Example extends EDDesign:
        |  val a = new RTDomain:
        |    val x = Bits(8) <> IN
        |    val y = Bits(8) <> OUT
        |    val v = Bits(8) <> VAR
        |    val v_ver1 = Bits(8) <> VAR
        |    v_ver1 := h"??"
        |    val v_ver2 = Bits(8) <> VAR
        |    v_ver2 := h"??"
        |    v := x
        |    v_ver1 := v
        |    report(s"v is ${v_ver1}", Severity.Warning)
        |    val a: Bits[8] <> CONST = h"05"
        |    v_ver2 := v
        |    assert(v_ver2 == a, s"bad ${v_ver2}")
        |    v := v | h"0f"
        |    y := v
        |  end a
        |end Example
        |""".stripMargin
    )
  }
  test("an unsettled guard path skips the site") {
    class Example extends EDDesign:
      val a = new RTDomain:
        val x = Bits(8) <> IN
        val y = Bit     <> OUT
        val q = Bits(8) <> OUT.REG
        val c = Bit     <> VAR
        val v = Bits(8) <> VAR
        v            := x
        c            := x(0)
        if (c) q.din := v
        v            := v | h"0f"
        c            := c || x(1)
        y            := c
    val top = (new Example).nameVarVersions
    assertCodeString(
      top,
      """class Example extends EDDesign:
        |  val a = new RTDomain:
        |    val x = Bits(8) <> IN
        |    val y = Bit <> OUT
        |    val q = Bits(8) <> OUT.REG
        |    val c = Bit <> VAR
        |    val v = Bits(8) <> VAR
        |    v := x
        |    c := x(0)
        |    if (c) q.din := v
        |    v := v | h"0f"
        |    c := c || x(1)
        |    y := c
        |  end a
        |end Example
        |""".stripMargin
    )
  }
  test("a pure sequential domain is untouched") {
    class Example extends EDDesign:
      val ram = Bits(8) X 4 <> VAR.SHARED
      val a   = new RTDomain:
        val data = Bits(8) <> IN
        val addr = Bits(2) <> IN
        val q    = Bits(8) <> OUT.REG
        val we   = Bit     <> IN
        q.din             := ram(addr)
        if (we) ram(addr) := data
    val top = (new Example).nameVarVersions
    assertCodeString(
      top,
      """class Example extends EDDesign:
        |  val ram = Bits(8) X 4 <> VAR.SHARED
        |  val a = new RTDomain:
        |    val data = Bits(8) <> IN
        |    val addr = Bits(2) <> IN
        |    val q = Bits(8) <> OUT.REG
        |    val we = Bit <> IN
        |    q.din := ram(addr.uint.toInt)
        |    if (we) ram(addr.uint.toInt) := data
        |  end a
        |end Example
        |""".stripMargin
    )
  }
  // a parametric-width bubble is a width-independent function over a single-element bubble
  // constant (`Bubble.constValOf`), so the don't-care default works for parametric types too
  test("parametric-width capture takes a repeated-bubble default") {
    class Example(val W: Int <> CONST = 8) extends EDDesign:
      val ram = Bits(W) X 4 <> VAR.SHARED
      val a   = new RTDomain:
        val x    = Bits(W) <> IN
        val we   = Bit     <> IN
        val addr = Bits(2) <> IN
        val y    = Bits(W) <> OUT
        val v    = Bits(W) <> VAR
        v                 := x
        if (we) ram(addr) := v
        v                 := v | x
        y                 := v
    val top = (new Example()).nameVarVersions
    assertCodeString(
      top,
      """class Example(val W: Int <> CONST = 8) extends EDDesign:
        |  val ram = Bits(W) X 4 <> VAR.SHARED
        |  val a = new RTDomain:
        |    val x = Bits(W) <> IN
        |    val we = Bit <> IN
        |    val addr = Bits(2) <> IN
        |    val y = Bits(W) <> OUT
        |    val v = Bits(W) <> VAR
        |    val v_ver = Bits(W) <> VAR
        |    v_ver := b"?".repeat(W)
        |    v := x
        |    if (we)
        |      v_ver := v
        |      ram(addr.uint.toInt) := v_ver
        |    end if
        |    v := v | x
        |    y := v
        |  end a
        |end Example
        |""".stripMargin
    )
  }
  // a single whole-target domain-level assignment lowers to a concurrent connection in `ToED`,
  // so reading the wire is settled at every position, including before the assignment: the
  // NameRegAliases-planted din write of the common counter idiom reads `cnt` before its only
  // assignment and needs no capture
  test("a connection-promoted wire needs no capture") {
    class Example extends EDDesign:
      val a = new RTDomain:
        val cnt = UInt(8) <> OUT init 0
        cnt := cnt.reg + 1
    val top = (new Example).nameVarVersions
    assertCodeString(
      top,
      """class Example extends EDDesign:
        |  val a = new RTDomain:
        |    val cnt = UInt(8) <> OUT
        |    val cnt_reg = UInt(8) <> VAR.REG init d"8'0"
        |    cnt_reg.din := cnt
        |    cnt := cnt_reg + d"8'1"
        |  end a
        |end Example
        |""".stripMargin
    )
  }
  // f(f(x)) == f(x): after the rewrite every capture read is settled, so a re-run finds no
  // trigger
  test("idempotency: a second run changes nothing") {
    class Example extends EDDesign:
      val ram = Bits(8) X 4 <> VAR.SHARED
      val a   = new RTDomain:
        val x    = Bits(8) <> IN
        val we   = Bit     <> IN
        val addr = Bits(2) <> IN
        val y    = Bits(8) <> OUT
        val v    = Bits(8) <> VAR
        v                 := x
        if (we) ram(addr) := v
        v                 := v | h"0f"
        y                 := v
    val once  = (new Example).nameVarVersions
    val twice = once.nameVarVersions
    import dfhdl.compiler.printing.DefaultPrinter
    assertNoDiff(
      {
        import twice.getSet
        DefaultPrinter.csDB
      }, {
        import once.getSet
        DefaultPrinter.csDB
      }
    )
  }
end NameVarVersionsSpec
