package dfhdl.compiler.stages

import dfhdl.compiler.ir
import dfhdl.compiler.printing.Printer
import dfhdl.core.Design
import dfhdl.options.{CompilerOptions, PrinterOptions}

final class StagedDesign[D <: Design] private (val design: D, val stagedDB: ir.DB)
object StagedDesign:
  def apply[D <: Design](design: D): StagedDesign[D] = new StagedDesign[D](design, design.getDB)
  extension [D <: Design](sd: StagedDesign[D])
    def compile(using
        co: CompilerOptions,
        po: PrinterOptions
    ): CompiledDesign[D] =
      co.backend.compile(sd).transform(designDB =>
        Printer.commit(designDB, co.compilePath(designDB))
      )
    def newStage(stagedDB: ir.DB): StagedDesign[D] = new StagedDesign[D](sd.design, stagedDB)
    def transform(transformDB: ir.DB => ir.DB): StagedDesign[D] =
      newStage(transformDB(sd.stagedDB))
    def addFiles(files: (Iterable[String] | String)*): StagedDesign[D] = ???
end StagedDesign
