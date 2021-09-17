package DFiant
import munit.*
import internals.{AllowTopLevel, HasTypeName}

class DFSpec extends FunSuite, AllowTopLevel, HasTypeName:
  final given dfc: DFC = core.DFC.empty
  private final val owner: core.DFOwner = core.DFDesign.Block(typeName)
  dfc.enterOwner(owner)
  transparent inline def assertCompileError(
      inline code: String,
      expectedErr: String
  ): Unit =
    assertNoDiff(
      compiletime.testing.typeCheckErrors(code).head.message,
      expectedErr
    )

  transparent inline def assertDSLError(expectedErr: String)(
      inline compileTimeCode: String
  )(runTimeCode: => Unit): Unit =
    assertCompileError(compileTimeCode, expectedErr)
    interceptMessage[java.lang.IllegalArgumentException](expectedErr) {
      runTimeCode
    }
end DFSpec
