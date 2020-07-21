package DFiant

import compiler.backend.BackendStage
package object compiler {
  implicit def evFixAnonymous[D <: DFDesign, C](c : C)(implicit conv : C => IRCompilation[D])
  : FixAnonymousOps[D] = new FixAnonymousOps[D](c)
  implicit def evUniqueDesigns[D <: DFDesign, C](c : C)(implicit conv : C => IRCompilation[D])
  : UniqueDesignsOps[D] = new UniqueDesignsOps[D](c)
  implicit def evNamedSelection[D <: DFDesign, C](c : C)(implicit conv : C => IRCompilation[D])
  : NamedSelectionOps[D] = new NamedSelectionOps[D](c)
  implicit def evUniqueNames[D <: DFDesign, C](c : C)(implicit conv : C => IRCompilation[D])
  : UniqueNamesOps[D] = new UniqueNamesOps[D](c)
  implicit def evFlattenNamesOps[D <: DFDesign, C](c : C)(implicit conv : C => IRCompilation[D])
  : FlattenNamesOps[D] = new FlattenNamesOps[D](c)
  implicit def evExplicitPrev[D <: DFDesign, C](c : C)(implicit conv : C => IRCompilation[D])
  : ExplicitPrevOps[D] = new ExplicitPrevOps[D](c)
  implicit def evExplicitConversions[D <: DFDesign, C](c : C)(implicit conv : C => IRCompilation[D])
  : ExplicitConversionsOps[D] = new ExplicitConversionsOps[D](c)
  implicit def evExplicitNamedVars[D <: DFDesign, C](c : C)(implicit conv : C => IRCompilation[D])
  : ExplicitNamedVarsOps[D] = new ExplicitNamedVarsOps[D](c)
  implicit def evViaPortConnection[D <: DFDesign, C](c : C)(implicit conv : C => IRCompilation[D])
  : ViaPortConnectionOps[D] = new ViaPortConnectionOps[D](c)
  implicit def evFlatten[D <: DFDesign, C](c : C)(implicit conv : C => IRCompilation[D])
  : FlattenOps[D] = new FlattenOps[D](c)
  implicit def evCalculator[D <: DFDesign, C](c : C)(implicit conv : C => IRCompilation[D])
  : CalculatorOps[D] = new CalculatorOps[D](c)
  implicit def evSingleStepPrev[D <: DFDesign, C](c : C)(implicit conv : C => IRCompilation[D])
  : SingleStepPrevOps[D] = new SingleStepPrevOps[D](c)
  
  trait PreCompiler[D <: DFDesign] {
    def apply(fromStage : IRCompilation[D]) : IRCompilation[D]
  }
  object PreCompiler {
    implicit def defaultDoesNothing[D <: DFDesign] : PreCompiler[D] =
      (fromStage : IRCompilation[D]) => fromStage
  }
  trait PostCompiler[D <: DFDesign, B <: BackendStage] {
    def apply(fromStage : BackendStage.Compilation[D, B]) : BackendStage.Compilation[D, B]
  }
  object PostCompiler {
    implicit def defaultDoesNothing[D <: DFDesign, B <: BackendStage] : PostCompiler[D, B] =
      (fromStage : BackendStage.Compilation[D, B]) => fromStage
  }
}
