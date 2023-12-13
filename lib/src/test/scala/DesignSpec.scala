package dfhdl
import dfhdl.compiler.ir.DB
import dfhdl.internals.AllowTopLevel
import dfhdl.compiler.stages.getCodeString
import munit.*

abstract class DesignSpec extends FunSuite, AllowTopLevel:
  extension (dsn: core.Design)
    def assertCodeString(cs: String): Unit =
      assertNoDiff(dsn.getCodeString, cs)
