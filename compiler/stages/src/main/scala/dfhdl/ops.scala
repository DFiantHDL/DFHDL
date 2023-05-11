package dfhdl

import dfhdl.compiler.stages.{BackendCompiler, StagedDesign, CompiledDesign}
import dfhdl.core.Design

export compiler.stages.printCodeString

extension [D <: Design](dsn: D)
  def compile(using bc: BackendCompiler): CompiledDesign[D] = bc(
    new StagedDesign[D](dsn, dsn.getDB)
  )
