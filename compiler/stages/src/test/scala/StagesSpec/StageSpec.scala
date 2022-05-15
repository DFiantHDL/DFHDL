package StagesSpec

import dfhdl.compiler.ir.DB
import dfhdl.compiler.printing.{DefaultPrinter, Printer, codeString}
import dfhdl.internals.AllowTopLevel
import munit.*

abstract class StageSpec extends FunSuite, AllowTopLevel:
  def assertCodeString(db: DB, cs: String): Unit =
    import db.getSet
    given Printer = DefaultPrinter
    assertNoDiff(db.codeString, cs)

end StageSpec
