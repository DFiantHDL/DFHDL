package CoreSpec
import DFiant.*
import munit.*
import internals.{AllowTopLevel, HasTypeName, Position}
import compiler.printing.{DefaultPrinter, Printer}
import core.HasDFC

abstract class DFSpec extends FunSuite, AllowTopLevel, HasTypeName, HasDFC:
  final val dfc: DFC = core.DFC.empty
  type TScope = core.DFC.Scope.Design
  given TScope = core.DFC.Scope.Design
  type TDomain = core.DFC.Domain.DF
  given TDomain = core.DFC.Domain.DF
  given printer: Printer = DefaultPrinter(using dfc.getSet)
  private final val owner: core.Design.Block =
    core.Design.Block(compiler.ir.DomainType.DF, typeName, Position.unknown)
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

  def assertRuntimeError(expectedErr: String)(runTimeCode: => Unit): Unit =
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

  transparent inline def assertDSLErrorLog(expectedErr: String)(
      inline compileTimeCode: String
  )(runTimeCode: => Unit)(using DFC): Unit =
    assertCompileError(expectedErr)(compileTimeCode)
    dfc.clearErrors()
    runTimeCode
    val err = dfc.getErrors.headOption.map(_.dfMsg).getOrElse(noErrMsg)
    dfc.clearErrors()
    assertNoDiff(err, expectedErr)

  def getCodeStringFrom(block: => Unit): String =
    import dfc.getSet
    val startIdx = dfc.mutableDB.getMembersSize
    block
    val endIdx = dfc.mutableDB.getMembersSize
    val members =
      dfc.mutableDB
        .getMembers(startIdx, endIdx)
        .filter(_.getOwner == dfc.owner.asIR)
    printer.csDFMembers(members)

  def printCodeString(block: => Unit): Unit =
    println(getCodeStringFrom(block))

  def assertCodeString(expectedCS: String)(block: => Unit): Unit =
    val cs = getCodeStringFrom(block)
    assertNoDiff(cs, expectedCS)

end DFSpec
