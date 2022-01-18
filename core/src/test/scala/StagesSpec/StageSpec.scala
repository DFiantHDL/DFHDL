package StagesSpec

import DFiant.compiler.ir.DB
import DFiant.compiler.printing.{DefaultPrinter, Printer, codeString}
import DFiant.internals.AllowTopLevel
import munit.*

class StageSpec extends FunSuite, AllowTopLevel:
  def assertCodeString(db: DB, cs: String): Unit =
    import db.getSet
    given Printer = DefaultPrinter
    assertNoDiff(db.codeString, cs)

end StageSpec
