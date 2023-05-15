package dfhdl.compiler.stages

import dfhdl.core.Design
import dfhdl.options.PrinterOptions
import dfhdl.compiler.ir

opaque type CommittedDesign[D <: Design] = CompiledDesign[D]
object CommittedDesign:
  def apply[D <: Design](cd: CompiledDesign[D]): CommittedDesign[D] = cd
  extension [D <: Design](cd: CommittedDesign[D])
    def staged: StagedDesign[D] =
      import CompiledDesign.staged as cds
      cd.cds
    def stagedDB: ir.DB = staged.stagedDB
    def newStage(stagedDB: ir.DB): CompiledDesign[D] =
      CompiledDesign(cd.staged.newStage(stagedDB))
    private def compiled: CompiledDesign[D] = cd
    def printGenFiles(using PrinterOptions): CommittedDesign[D] =
      import CompiledDesign.printGenFiles as pgf
      compiled.pgf
    def addFiles(files: (Iterable[String] | String)*): CommittedDesign[D] =
      import CompiledDesign.addFiles as addFiles2
      compiled.addFiles2(files*)
  end extension
end CommittedDesign
