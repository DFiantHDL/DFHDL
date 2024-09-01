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
