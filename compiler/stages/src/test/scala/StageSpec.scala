package dfhdl
import dfhdl.compiler.ir.DB
import dfhdl.compiler.printing.DefaultPrinter
import dfhdl.compiler.stages.{sanityCheck, dropUnreferencedAnons}
import dfhdl.internals.NoTopAnnotIsRequired
import munit.*

abstract class StageSpec(stageCreatesUnrefAnons: Boolean = false)
    extends FunSuite,
      NoTopAnnotIsRequired:
  inline def assertCodeString(db: DB, cs: String): Unit =
    import db.getSet
    if (stageCreatesUnrefAnons) db.dropUnreferencedAnons.sanityCheck
    else db.sanityCheck
    assertNoDiff(DefaultPrinter.csDB, cs)

end StageSpec
