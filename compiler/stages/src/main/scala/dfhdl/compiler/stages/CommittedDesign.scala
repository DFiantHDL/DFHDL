package dfhdl.compiler.stages

import dfhdl.core.Design

opaque type CommittedDesign[D <: Design] = CompiledDesign[D]
object CommittedDesign:
  def apply[D <: Design](cd: CompiledDesign[D]): CommittedDesign[D] = cd
  extension [D <: Design](cd: CommittedDesign[D])
    def staged: StagedDesign[D] =
      import CompiledDesign.staged as cds
      cd.cds
    private def compiled: CompiledDesign[D] = cd
    def printGenFiles: CommittedDesign[D] =
      import CompiledDesign.printGenFiles as pgf
      compiled.pgf
  end extension
end CommittedDesign
