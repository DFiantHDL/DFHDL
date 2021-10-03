package DFiant
import munit.*
import internals.{AllowTopLevel, HasTypeName}
import compiler.printing.{DefaultPrinter, Printer}

abstract class DFSpec extends FunSuite, AllowTopLevel, HasTypeName:
  final given dfc: DFC = core.DFC.empty
  given printer: Printer = DefaultPrinter
  private final val owner: core.DFOwner = core.DFDesign.Block(typeName)
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

  def assertCodeString(expectedCS: String)(block: => Unit): Unit =
    val startIdx = dfc.mutableDB.getMembersSize
    block
    val endIdx = dfc.mutableDB.getMembersSize
    val members = dfc.mutableDB.getMembers(startIdx, endIdx)
    val cs = printer.csDFMembers(members, false)(using dfc.getSet)
    assertNoDiff(cs, expectedCS)

end DFSpec
