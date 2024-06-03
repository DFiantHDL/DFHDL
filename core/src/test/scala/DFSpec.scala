package dfhdl
import munit.*
import internals.{AllowTopLevel, HasTypeName, Position, metaContextIgnore}
import compiler.printing.{DefaultPrinter, Printer}
import core.{HasDFC, DFValOf, DFValAny, DFConstOf, IntParam, IntP, Width, widthIntParam}
import compiler.ir
import ir.DFDesignBlock.InstMode
import java.nio.file._

extension [T](t: T)(using tc: core.DFType.TC[T])
  @metaContextIgnore
  def verifyWidth[R <: IntP](
      r: IntParam[R]
  )(using dfc: DFC, w: Width[tc.Type])(using w.Out =:= R): Unit =
    assert(t.widthIntParam.toScalaInt == r.toScalaInt)

extension [T <: DFType](t: DFValOf[T])(using dfc: DFC, w: Width[T])
  @metaContextIgnore
  def verifyWidth[R <: IntP](
      r: IntParam[R]
  )(using w.Out =:= R): Unit =
    assert(t.widthIntParam.toScalaInt == r.toScalaInt)

abstract class DFSpec extends FunSuite, AllowTopLevel, HasTypeName, HasDFC:
  final lazy val dfc: DFC = core.DFC.empty
  type TScope = core.DFC.Scope.Design
  given TScope = core.DFC.Scope.Design
  type TDomain = core.DomainType.DF
  given TDomain = core.DomainType.DF
  given dfPrinter: Printer = DefaultPrinter(using dfc.getSet)
  private final val owner: core.Design.Block =
    core.Design.Block(
      ir.DomainType.DF,
      ir.Meta(Some(typeName), Position.unknown, None, Nil),
      InstMode.Normal
    )
  dfc.enterOwner(owner)
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
    dfc.clearErrors()
    runTimeCode
    val err = dfc.getErrors.headOption.map(_.dfMsg).getOrElse(noErrMsg)
    assertNoDiff(err, expectedErr)

  transparent inline def assertDSLError(expectedErr: String)(
      inline compileTimeCode: String
  )(runTimeCode: => Unit): Unit =
    assertCompileError(expectedErr)(compileTimeCode)
    val err =
      try
        runTimeCode
        noErrMsg
      catch case e: IllegalArgumentException => e.getMessage
    assertNoDiff(err, expectedErr)

  def assertEquals[T <: DFType, L <: DFConstOf[T], R <: DFConstOf[T]](l: L, r: R): Unit =
    assert((l == r).toScalaBoolean)

  transparent inline def assertDSLErrorLog(expectedErr: String)(
      inline compileTimeCode: String
  )(runTimeCode: => Unit)(using DFC): Unit =
    val ignore =
      inline if (compileTimeCode != "")
        assertCompileError(expectedErr)(compileTimeCode)
      else ()
    dfc.clearErrors()
    runTimeCode
    val err = dfc.getErrors.headOption.map(_.dfMsg).getOrElse(noErrMsg)
    dfc.clearErrors()
    assertNoDiff(err, expectedErr)

  def getCodeStringFrom(block: => Unit): String =
    import dfc.getSet
    val startIdx = dfc.mutableDB.DesignContext.getMembersNum
    block
    val endIdx = dfc.mutableDB.DesignContext.getMembersNum
    val members =
      dfc.mutableDB.DesignContext
        .getMembers(startIdx, endIdx)
        .filter(_.getOwner == dfc.owner.asIR)
    dfPrinter.csDFMembers(members)

  def printCodeString(block: => Unit): Unit =
    println(getCodeStringFrom(block))

  inline def assertCodeString(expectedCS: String)(block: => Unit): Unit =
    val cs = getCodeStringFrom(block)
    assertNoDiff(cs, expectedCS)

  private def getCurrentNameAndLine(idx: Int): (String, Int) =
    val stackTrace = Thread.currentThread().getStackTrace
    val elm = stackTrace(idx)
    (elm.getFileName(), elm.getLineNumber)

  private def getFileNameFromPath(filePath: String): String =
    val path = Paths.get(filePath)
    path.getFileName.toString

  def assertLatestDesignDclPosition(
      lineOffset: Int,
      lineCount: Int,
      colStart: Int,
      colEnd: Int
  ): Unit =
    dfc.mutableDB.DesignContext.getLastDesignInst.dclMeta.assertPosition(
      lineOffset,
      lineCount,
      colStart,
      colEnd
    )

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

end DFSpec
