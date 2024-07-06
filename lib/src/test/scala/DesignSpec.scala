package dfhdl
import dfhdl.compiler.ir.DB
import dfhdl.internals.AllowTopLevel
import dfhdl.compiler.stages.getCodeString
import munit.*

abstract class DesignSpec extends FunSuite, AllowTopLevel:
  extension (dsn: core.Design)
    inline def assertCodeString(cs: String): Unit =
      assertNoDiff(dsn.getCodeString, cs)

  private val noErrMsg = "No error found"

  inline def assertElaborationErrors(dsn: => core.Design)(expectedErr: String): Unit =
    val err =
      try
        dsn
        noErrMsg
      catch case e: IllegalArgumentException => e.getMessage
    assertNoDiff(err, expectedErr)
