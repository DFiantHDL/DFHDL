package DFiant
import munit.*
import internals.{AllowTopLevel, HasTypeName, Position}
import compiler.printing.{DefaultPrinter, Printer}
import core.HasDFC

abstract class DFSpec extends FunSuite, AllowTopLevel, HasTypeName, HasDFC:
  final val dfc: DFC = core.DFC.empty
  given printer: Printer = DefaultPrinter
  private final val owner: core.DFDesign.Block = core.DFDesign.Block(typeName, Position.unknown)
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

  def getCodeStringFrom(block: => Unit): String =
    import dfc.getSet
    val startIdx = dfc.mutableDB.getMembersSize
    block
    val endIdx = dfc.mutableDB.getMembersSize
    val members =
      dfc.mutableDB
        .getMembers(startIdx, endIdx)
        .filter(_.getOwner == dfc.owner.asIR)
    printer.csDFMembers(members, false)(using dfc.getSet)

  def printCodeString(block: => Unit): Unit =
    println(getCodeStringFrom(block))

  def assertCodeString(expectedCS: String)(block: => Unit): Unit =
    val cs = getCodeStringFrom(block)
    assertNoDiff(cs, expectedCS)

end DFSpec
