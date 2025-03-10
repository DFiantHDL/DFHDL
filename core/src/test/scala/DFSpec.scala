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

abstract class DFSpec extends NoDFCSpec, AllowTopLevel, HasTypeName, HasDFC:
  final lazy val dfc: DFC = core.DFC.emptyNoEO
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

  inline def assertRuntimeErrorLog(expectedErr: String)(runTimeCode: => Unit): Unit =
    dfc.clearErrors()
    runTimeCode
    val err = dfc.getErrors.headOption.map(_.dfMsg).getOrElse(noErrMsg)
    assertNoDiff(err, expectedErr)

  def assertEquals[T <: DFType, L <: DFConstOf[T], R <: DFConstOf[T]](l: L, r: R)(using DFC): Unit =
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
