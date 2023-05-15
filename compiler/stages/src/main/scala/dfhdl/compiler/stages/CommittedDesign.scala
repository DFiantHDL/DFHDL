package dfhdl.compiler.stages

import dfhdl.core.Design
import dfhdl.options.PrinterOptions

opaque type CommittedDesign[D <: Design] = CompiledDesign[D]
object CommittedDesign:
  def apply[D <: Design](cd: CompiledDesign[D]): CommittedDesign[D] = cd
  extension [D <: Design](cd: CommittedDesign[D])
    def staged: StagedDesign[D] =
      import CompiledDesign.staged as cds
      cd.cds
    private def compiled: CompiledDesign[D] = cd
    def printGenFiles(using PrinterOptions): CommittedDesign[D] =
      import CompiledDesign.printGenFiles as pgf
      compiled.pgf
    def addFiles(files: (Iterable[String] | String)*): CommittedDesign[D] =
      compiled.addFiles(files*)
  end extension
end CommittedDesign
