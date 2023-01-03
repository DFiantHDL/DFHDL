package StagesSpec

import dfhdl.compiler.ir.DB
import dfhdl.compiler.printing.DefaultPrinter
import dfhdl.internals.AllowTopLevel
import munit.*

abstract class StageSpec extends FunSuite, AllowTopLevel:
  def assertCodeString(db: DB, cs: String): Unit =
    import db.getSet
    assertNoDiff(DefaultPrinter.csDB, cs)

end StageSpec
