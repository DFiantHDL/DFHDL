package dfhdl
import munit.*
import internals.{Position, metaContextIgnore, NoTopAnnotIsRequired}
import core.{DFValAny, DFConstOf}
import compiler.ir
import java.nio.file._

abstract class NoDFCSpec extends FunSuite, NoTopAnnotIsRequired:
  private val noErrMsg = "No error found"
  transparent inline def assertCompileError(expectedErr: String)(
      inline code: String
  ): Unit =
    val err = compiletime.testing.typeCheckErrors(code) match
      case (_ :+ last) => last.message
      case _           => noErrMsg
    assertNoDiff(
      err,
      expectedErr
    )
  end assertCompileError

  // TODO: there is a problem in DFDecimalSpec position error check
  transparent inline def assertCompileErrorPos(expectedErr: String, column: Int)(
      inline code: String
  ): Unit =
    val err = compiletime.testing.typeCheckErrors(code) match
      case x @ (_ :+ last) =>
        scala.Predef.println(x.map(_.column))
        scala.Predef.println(x.map(_.lineContent))
        last.message
      case _ => noErrMsg
    assertNoDiff(
      err,
      expectedErr
    )
  end assertCompileErrorPos

  // Like `assertCompileError`, but the snippet is compiled through the DFHDL plugin phases as
  // well, so diagnostics that only they emit are surfaced (`typeCheckErrors` stops at the
  // typer). The snippet must be a self-contained block: it gets `import dfhdl.*` prepended,
  // but sees the classpath rather than the call site's lexical scope.
  // See devdocs/plugin-error-testing.md.
  transparent inline def assertPluginError(expectedErr: String)(
      inline code: String
  ): Unit =
    val errs = internals.PluginErrCheck.pluginCheckErrors(code)
    assertNoDiff(
      errs.lastOption.getOrElse(noErrMsg),
      expectedErr
    )
  end assertPluginError

  // Like `assertPluginError`, but asserts the snippet produces EXACTLY one error with the given
  // user-facing text: on top of the message itself, this pins the diagnostic dedup (an
  // inline-expansion error re-raised at several positions must render once).
  transparent inline def assertSinglePluginError(expectedErr: String)(
      inline code: String
  ): Unit =
    val errs = internals.PluginErrCheck.pluginCheckErrors(code)
    val actual = errs match
      case single :: Nil => single
      case Nil           => noErrMsg
      case many          => many.mkString("\n===== MULTIPLE ERRORS =====\n")
    assertNoDiff(actual, expectedErr)
  end assertSinglePluginError

  inline def assertRuntimeError(expectedErr: String)(runTimeCode: => Unit): Unit =
    val err =
      try
        runTimeCode
        noErrMsg
      catch case e: IllegalArgumentException => e.getMessage
    assertNoDiff(err, expectedErr)

  transparent inline def assertDSLError(expectedErr: String)(
      inline compileTimeCode: String
  )(runTimeCode: => Unit): Unit =
    assertCompileError(expectedErr)(compileTimeCode)
    assertRuntimeError(expectedErr)(runTimeCode)

  private def getCurrentNameAndLine(idx: Int): (String, Int) =
    val stackTrace = Thread.currentThread().getStackTrace
    val elm = stackTrace(idx)
    (elm.getFileName(), elm.getLineNumber)

  private def getFileNameFromPath(filePath: String): String =
    val path = Paths.get(filePath)
    path.getFileName.toString

  extension (meta: compiler.ir.Meta)
    def assertPosition(lineOffset: Int, lineCount: Int, colStart: Int, colEnd: Int): Unit =
      val (fileName, line) = getCurrentNameAndLine(4)
      val expectedPositionStr =
        s"$fileName:${line - lineCount + 1 - lineOffset}:$colStart - ${line - lineOffset}:$colEnd"
      val currentPosition = meta.position
      val positionNoPath = currentPosition.copy(file = getFileNameFromPath(currentPosition.file))
      assertNoDiff(positionNoPath.toString, expectedPositionStr)
  end extension

  extension (dfVal: ir.DFMember)
    @metaContextIgnore
    def assertPosition(lineOffset: Int, lineCount: Int, colStart: Int, colEnd: Int): Unit =
      dfVal.meta.assertPosition(lineOffset, lineCount, colStart, colEnd)
  end extension
  extension (dfVal: DFValAny)
    @metaContextIgnore
    def assertPosition(lineOffset: Int, lineCount: Int, colStart: Int, colEnd: Int): Unit =
      dfVal.asIR.meta.assertPosition(lineOffset, lineCount, colStart, colEnd)
  end extension
end NoDFCSpec
