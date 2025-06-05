package dfhdl.compiler.stages

import dfhdl.compiler.ir
import dfhdl.compiler.printing.Printer
import dfhdl.core.Design
import dfhdl.options.{CompilerOptions, PrinterOptions}

final class StagedDesign(val stagedDB: ir.DB)
object StagedDesign:
  def apply(design: Design): StagedDesign = new StagedDesign(design.getDB)
  extension (sd: StagedDesign)
    def compile(using co: CompilerOptions, po: PrinterOptions): CompiledDesign =
      co.backend.compile(sd).commit
    def newStage(stagedDB: ir.DB): StagedDesign = new StagedDesign(stagedDB)
    def transform(transformDB: ir.DB => ir.DB): StagedDesign =
      newStage(transformDB(sd.stagedDB))
    def addFiles(files: (Iterable[String] | String)*): StagedDesign = ???
end StagedDesign
