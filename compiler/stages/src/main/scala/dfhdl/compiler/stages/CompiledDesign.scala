package dfhdl.compiler.stages

import dfhdl.compiler.printing.Printer
import dfhdl.core.Design
import dfhdl.options.{PrinterOptions, CompilerOptions}
import dfhdl.compiler.ir
import java.nio.file.{Paths, Files}

opaque type CompiledDesign = StagedDesign
object CompiledDesign:
  def apply(sd: StagedDesign): CompiledDesign = sd
  extension (cd: CompiledDesign)
    def staged: StagedDesign = cd
    def stagedDB: ir.DB = staged.stagedDB
    def newStage(stagedDB: ir.DB): CompiledDesign =
      import StagedDesign.newStage as newStage2
      CompiledDesign(staged.newStage2(stagedDB))
    def transform(transformDB: ir.DB => ir.DB): CompiledDesign =
      import StagedDesign.transform as transform2
      CompiledDesign(staged.transform2(transformDB))
    def printBackendCode(using co: CompilerOptions, po: PrinterOptions): CompiledDesign =
      Printer.printBackendCode(co.backend.printer(staged.stagedDB))
      cd
    def commit(using co: CompilerOptions): CompiledDesign =
      cd.transform(designDB => Printer.commit(designDB, co.topCommitPath(designDB)))
    def addFiles(files: (Iterable[String] | String)*): CompiledDesign =
      import StagedDesign.addFiles as addFiles2
      staged.addFiles2(files*)
  end extension
end CompiledDesign
