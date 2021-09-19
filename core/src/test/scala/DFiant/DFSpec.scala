package DFiant
import munit.*
import internals.{AllowTopLevel, HasTypeName}

abstract class DFSpec extends FunSuite, AllowTopLevel, HasTypeName:
  final given dfc: DFC = core.DFC.empty
  private final val owner: core.DFOwner = core.DFDesign.Block(typeName)
  dfc.enterOwner(owner)
  private val noErrMsg = "No error found"
  transparent inline def assertCompileError(
      inline code: String,
      expectedErr: String
  ): Unit =
    val err = compiletime.testing.typeCheckErrors(code) match
      case (head :: _) => head.message
      case Nil         => noErrMsg
    assertNoDiff(
      err,
      expectedErr
    )
  end assertCompileError

  transparent inline def assertDSLError(expectedErr: String)(
      inline compileTimeCode: String
  )(runTimeCode: => Unit): Unit =
    assertCompileError(compileTimeCode, expectedErr)
    val err =
      try
        runTimeCode
        noErrMsg
      catch case e: IllegalArgumentException => e.getMessage
    assertNoDiff(err, expectedErr)
end DFSpec
