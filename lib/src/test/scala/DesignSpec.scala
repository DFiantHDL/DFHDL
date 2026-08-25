package dfhdl
import dfhdl.compiler.ir.DB
import dfhdl.compiler.stages.getCodeString
import munit.*

abstract class DesignSpec extends FunSuite:
  extension (dsn: core.Design)
    inline def assertCodeString(cs: String): Unit =
      assertNoDiff(dsn.getCodeString, cs)

  private val noErrMsg = "No error found"

  // Every same-file `<file>.scala:LINE:COL` in the obtained error message is rewritten so that
  // LINE becomes an offset from the asserting call's anchor line: `L-9` reads "nine lines above
  // this assertion". The anchor is what `munit.Location` reports for the call, which is the
  // line its LAST argument-list paren CLOSES on, not the line it opens on. Expected strings are
  // authored in this relative form, which keeps them immune to line churn anywhere else in the
  // spec file; only edits inside the test itself (between its designs and its assertion's
  // closing paren) move the offsets, and a failure's diff shows the correct relative form to
  // paste. Positions in other files are left absolute.
  protected def relativizeLines(msg: String)(using loc: Location): String =
    val fileName = loc.path.split("[/\\\\]").last
    val re = (java.util.regex.Pattern.quote(fileName) + """:(\d+):(\d+) - (\d+):(\d+)""").r
    def rel(numStr: String): String =
      val d = numStr.toInt - loc.line
      if (d < 0) s"L-${-d}" else s"L+$d"
    re.replaceAllIn(
      msg,
      m =>
        scala.util.matching.Regex.quoteReplacement(
          s"$fileName:${rel(m.group(1))}:${m.group(2)} - ${rel(m.group(3))}:${m.group(4)}"
        )
    )
  end relativizeLines

  inline def assertElaborationErrors(
      dsn: => core.Design
  )(expectedErr: String)(using loc: Location): Unit =
    val err =
      try
        dsn
        noErrMsg
      catch case e: IllegalArgumentException => e.getMessage
    assertNoDiff(
      relativizeLines(err)(using loc),
      expectedErr,
      "diff assertion failed (positions are relative: `L-n` means n lines above this " +
        "assertion's closing paren, the line munit reports for it)"
    )
end DesignSpec
