package dfhdl.compiler.stages

import dfhdl.compiler.ir
import dfhdl.core.Design

final class StagedDesign[D <: Design](val design: D, val stagedDB: ir.DB)
object StagedDesign:
  extension [D <: Design](sd: StagedDesign[D])
    def compile(using bc: BackendCompiler): CompiledDesign[D] = bc(sd)
    def newStage(stagedDB: ir.DB): StagedDesign[D] = new StagedDesign[D](sd.design, stagedDB)
