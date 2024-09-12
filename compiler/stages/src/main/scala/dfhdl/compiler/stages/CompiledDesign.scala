package dfhdl.compiler.stages

import dfhdl.compiler.printing.Printer
import dfhdl.core.Design
import dfhdl.options.{PrinterOptions, CompilerOptions}
import dfhdl.compiler.ir
import java.nio.file.{Paths, Files}

opaque type CompiledDesign[D <: Design] = StagedDesign[D]
object CompiledDesign:
  def apply[D <: Design](sd: StagedDesign[D]): CompiledDesign[D] = sd
  extension [D <: Design](cd: CompiledDesign[D])
    def staged: StagedDesign[D] = cd
    def stagedDB: ir.DB = staged.stagedDB
    def newStage(stagedDB: ir.DB): CompiledDesign[D] =
      import StagedDesign.newStage as newStage2
      CompiledDesign(staged.newStage2(stagedDB))
    def transform(transformDB: ir.DB => ir.DB): CompiledDesign[D] =
      import StagedDesign.transform as transform2
      CompiledDesign(staged.transform2(transformDB))
    def printBackendCode(using PrinterOptions): CompiledDesign[D] =
      Printer.printBackendCode(staged.stagedDB)
      cd
    def commit(using co: CompilerOptions): CompiledDesign[D] =
      cd.transform(designDB => Printer.commit(designDB, co.compilePath(designDB)))
    def addFiles(files: (Iterable[String] | String)*): CompiledDesign[D] =
      import StagedDesign.addFiles as addFiles2
      staged.addFiles2(files*)
  end extension
end CompiledDesign
