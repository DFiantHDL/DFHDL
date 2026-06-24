package app
import munit.*
import java.io.{ByteArrayOutputStream, PrintStream}

// Integration tests that drive the plugin-injected `TestCLIFoo` entry point
// (the `main` synthesized into its companion object).
//
// Each `main` call instantiates a fresh `DFApp`, so its `elaborate` Step (a
// lazy val on that instance) is per-invocation — there is no cross-call state
// to reset. We still drive every supported type through a single comprehensive
// override test; stateless tests (help output, malformed literal rejection)
// run freely alongside it.
//
// Per-type unit coverage for `updateScalaValue` / `defaultDisplay` lives in
// `DesignArgSpec.scala`; this file just verifies the scallop wiring and the
// end-to-end propagation of CLI overrides into elaboration.
class DesignArgsCLISpec extends FunSuite:

  // Run `TestCLIFoo.main` against a synthetic CLI argv and return the
  // captured stdout + stderr. DFApp uses a wvlet logger that writes directly
  // to System.err, so we redirect System-level streams (not just the Scala
  // `Console` facade) to capture everything. The actual reflective call is in
  // `DesignArgsCLIHelper.java` — keeping it in a Java file avoids the DFHDL
  // plugin rewriting the reflection calls with meta-context forwarders. Each
  // invocation builds a fresh `DFApp`, so help-formatting tests already see a
  // pristine synthetic-default state without any save/restore.
  // Matches ANSI CSI / SGR escape sequences so our substring assertions
  // operate on clean text. The wvlet logger wraps lines with `\u001b[0J`
  // (erase-below) and similar control codes that otherwise pollute the output.
  private val ansiRe = "\u001b\\[[;\\d]*[A-Za-z]".r

  private def runCLI(args: String*): String =
    val buf = new ByteArrayOutputStream()
    val ps = new PrintStream(buf, /*autoFlush = */ true, "UTF-8")
    val savedOut = System.out
    val savedErr = System.err
    System.setOut(ps)
    System.setErr(ps)
    try
      Console.withOut(ps):
        Console.withErr(ps):
          DesignArgsCLIHelper.invokeTopTestCLIFoo(args.toArray)
    finally
      System.setOut(savedOut)
      System.setErr(savedErr)
    ansiRe.replaceAllIn(buf.toString("UTF-8"), "")
  end runCLI

  // --------------------------------------------------------------------
  // 1. Help output: every param listed, synthetic defaults suppressed,
  //    user defaults shown, empty string rendered as `""`.
  // --------------------------------------------------------------------
  test("help lists every parameter with its type and only user defaults"):
    val out = runCLI("help")
    assert(clue(out).contains("Design arguments:"))
    assertContainsLine(out, List("--a", "<Int>"), negative = false)
    assertContainsLine(out, List("--a", "(default ="), negative = true)
    assertContainsLine(out, List("--c", "<Bit>"), negative = false)
    assertContainsLine(out, List("--c", "(default ="), negative = true)
    assertContainsLine(out, List("--d", "<String>"), negative = false)
    assertContainsLine(out, List("--d", "(default ="), negative = true)
    assertContainsLine(out, List("--e", "<Double>"), negative = false)
    assertContainsLine(out, List("--e", "(default ="), negative = true)
    assertContainsLine(out, List("--f", "<Bits>"), negative = false)
    assertContainsLine(out, List("--f", "(default ="), negative = true)
    assertContainsLine(out, List("--g", "<UInt>"), negative = false)
    assertContainsLine(out, List("--g", "(default ="), negative = true)
    assertContainsLine(out, List("--h", "<SInt>"), negative = false)
    assertContainsLine(out, List("--h", "(default ="), negative = true)
    // User default on `b` must show the default.
    assertContainsLine(out, List("--b", "<Boolean>", "(default = true)"), negative = false)
    // Regression guard for the old empty-String bug — the design-arg section
    // must never show a bare `(default = )` (empty value). Other parts of the
    // help screen — e.g. `--global-defs-name` — may legitimately display an
    // empty default, so scope the check to the design-arg lines only.
    val designArgLines = out.linesIterator
      .dropWhile(!_.contains("Design arguments:"))
      .drop(1)
      .takeWhile(l => !(l.startsWith("Mode: ") || l.startsWith("      --cache")))
      .mkString("\n")
    assert(
      !designArgLines.contains("(default = )"),
      s"Found bare `(default = )` in design-arg help:\n$designArgLines"
    )

  // --------------------------------------------------------------------
  // 2. One comprehensive override call drives every supported type through
  //    the full CLI → ParsedCommandLine → DFApp → elaborate pipeline.
  //    The assertion verifies that each override value appears on the class
  //    header printed by `--print-elaborate`, proving the CLI value survived
  //    the round-trip and the synthetic-default tag fix is still in place.
  // --------------------------------------------------------------------
  test("every supported type propagates from CLI into elaborated design"):
    val out = runCLI(
      "--nocache",
      "--a", "5",
      "--b", "false",
      "--c", "1",
      "--d", "hello",
      "--e", "3.14",
      "--f", "h\"5a\"",
      "--g", "d\"16'100\"",
      "--h", "sd\"32'-42\"",
      "elaborate", "--print-elaborate"
    )
    assert(clue(out).contains("val a: Int      <> CONST = 5,"))
    assert(clue(out).contains("val b: Boolean  <> CONST = false,"))
    assert(clue(out).contains("val c: Bit      <> CONST = 1,"))
    assert(clue(out).contains("val d: String   <> CONST = \"hello\","))
    assert(clue(out).contains("val e: Double   <> CONST = 3.14,"))
    assert(clue(out).contains("val f: Bits[8]  <> CONST = h\"5a\","))
    assert(clue(out).contains("val g: UInt[16] <> CONST = d\"16'100\","))
    assert(clue(out).contains("val h: SInt[32] <> CONST = sd\"32'-42\""))

  // --------------------------------------------------------------------
  // 3. Malformed literals fail loudly. These never reach elaboration so
  //    the `elaborate`-cache limitation doesn't apply.
  // --------------------------------------------------------------------
  test("malformed Boolean literal surfaces a clear parse error"):
    val ex = intercept[IllegalArgumentException]:
      runCLI("--nocache", "--b", "maybe", "elaborate")
    assert(clue(ex.getMessage).contains("b"))
    assert(clue(ex.getMessage).toLowerCase.contains("boolean"))

  test("malformed Bits literal surfaces a clear parse error"):
    val ex = intercept[IllegalArgumentException]:
      runCLI("--nocache", "--f", "zzz", "elaborate")
    assert(clue(ex.getMessage).contains("f"))

  // --------------------------------------------------------------------
  // Helpers
  // --------------------------------------------------------------------
  private def assertContainsLine(
      haystack: String,
      needles: List[String],
      negative: Boolean
  )(using munit.Location): Unit =
    val matchingLine =
      haystack.linesIterator.find(line => needles.forall(line.contains))
    (matchingLine, negative) match
      case (Some(line), true) =>
        fail(s"Unexpected line matched all of ${needles.mkString(", ")}:\n  $line")
      case (None, false) =>
        fail(s"No line contains all of: ${needles.mkString(", ")}\n--- Output ---\n$haystack")
      case _ =>

end DesignArgsCLISpec
