package dfhdl
import munit.*
import internals.{HasTypeName, Position, metaContextIgnore}
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
    assert(t.widthIntParam.toScalaIntOpt.get == r.toScalaIntOpt.get)

extension [T <: DFType](t: DFValOf[T])(using dfc: DFC, w: Width[T])
  @metaContextIgnore
  def verifyWidth[R <: IntP](
      r: IntParam[R]
  )(using w.Out =:= R): Unit =
    assert(t.widthIntParam.toScalaIntOpt.get == r.toScalaIntOpt.get)

abstract class DFSpec extends NoDFCSpec, HasTypeName, HasDFC:
  final lazy val dfc: DFC = core.DFC.emptyNoEO
  type TScope = core.DFC.Scope.Design
  given TScope = core.DFC.Scope.Design
  type TDomain = core.DomainType.DF
  given TDomain = core.DomainType.DF
  given dfPrinter: Printer = DefaultPrinter(using dfc.getSet)
  private final val owner: core.Design.Block =
    core.Design.Block(ir.DomainType.DF, InstMode.Normal)
  dfc.enterOwner(owner)
  private val noErrMsg = "No error found"

  inline def assertRuntimeErrorLog(
      expectedErr: String,
      colStart: Int = -1,
      colEnd: Int = -1
  )(runTimeCode: => Unit): Unit =
    val currentEvents = dfc.getEvents
    dfc.clearEvents()
    runTimeCode
    val errOpt = dfc.getErrors.headOption
    val err = errOpt.map(_.dfMsg).getOrElse(noErrMsg)
    dfc.clearEvents()
    dfc.injectEvents(currentEvents)
    assertNoDiff(err, expectedErr)
    // optionally also verify the error position's column span
    if (colStart >= 0 || colEnd >= 0)
      val position = errOpt match
        case Some(err: core.DFError.Basic) => err.position
        case _                             => Position.unknown
      assertEquals(position.columnStart, colStart)
      assertEquals(position.columnEnd, colEnd)
  end assertRuntimeErrorLog

  def assertEquals[T <: DFType, L <: DFConstOf[T], R <: DFConstOf[T]](l: L, r: R)(using DFC): Unit =
    assert((l == r).toScalaBoolean)

  transparent inline def assertDSLErrorLog(expectedErr: String)(
      inline compileTimeCode: String
  )(runTimeCode: => Unit)(using DFC): Unit =
    val ignore =
      inline if (compileTimeCode != "")
        assertCompileError(expectedErr)(compileTimeCode)
      else ()
    val currentEvents = dfc.getEvents
    dfc.clearEvents()
    runTimeCode
    val err = dfc.getErrors.headOption.map(_.dfMsg).getOrElse(noErrMsg)
    dfc.clearEvents()
    dfc.injectEvents(currentEvents)
    assertNoDiff(err, expectedErr)

  inline def assertRuntimeWarningLog(expectedWarn: String)(runTimeCode: => Unit): Unit =
    val currentEvents = dfc.getEvents
    dfc.clearEvents()
    runTimeCode
    val warn = dfc.getWarnings.headOption.map(_.dfMsg).getOrElse(noErrMsg)
    dfc.clearEvents()
    dfc.injectEvents(currentEvents)
    assertNoDiff(warn, expectedWarn)

  def assertNoWarnings(): Unit =
    val warns = dfc.getWarnings
    if (warns.nonEmpty)
      assert(false, s"Expected no warnings, but found:\n${warns.mkString("\n")}")

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
    dfc.clearEvents()
    val cs = getCodeStringFrom(block)
    assertNoDiff(cs, expectedCS)
    val events = dfc.getEvents
    if (events.nonEmpty)
      assert(false, events.mkString("\n"))

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

end DFSpec
