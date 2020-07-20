package DFiant
import shapeless.HList

package object compiler {
  implicit def evFixAnonymous[D <: DFDesign, H <: shapeless.HList, C](c : C)(implicit conv : C => IRCompilation[D, H])
  : FixAnonymousOps[D, H] = new FixAnonymousOps[D, H](c)
  implicit def evUniqueDesigns[D <: DFDesign, H <: shapeless.HList, C](c : C)(implicit conv : C => IRCompilation[D, H])
  : UniqueDesignsOps[D, H] = new UniqueDesignsOps[D, H](c)
  implicit def evNamedSelection[D <: DFDesign, H <: shapeless.HList, C](c : C)(implicit conv : C => IRCompilation[D, H])
  : NamedSelectionOps[D, H] = new NamedSelectionOps[D, H](c)
  implicit def evUniqueNames[D <: DFDesign, H <: shapeless.HList, C](c : C)(implicit conv : C => IRCompilation[D, H])
  : UniqueNamesOps[D, H] = new UniqueNamesOps[D, H](c)
  implicit def evFlattenNamesOps[D <: DFDesign, H <: shapeless.HList, C](c : C)(implicit conv : C => IRCompilation[D, H])
  : FlattenNamesOps[D, H] = new FlattenNamesOps[D, H](c)
  implicit def evExplicitPrev[D <: DFDesign, H <: shapeless.HList, C](c : C)(implicit conv : C => IRCompilation[D, H])
  : ExplicitPrevOps[D, H] = new ExplicitPrevOps[D, H](c)
  implicit def evExplicitConversions[D <: DFDesign, H <: shapeless.HList, C](c : C)(implicit conv : C => IRCompilation[D, H])
  : ExplicitConversionsOps[D, H] = new ExplicitConversionsOps[D, H](c)
  implicit def evExplicitNamedVars[D <: DFDesign, H <: shapeless.HList, C](c : C)(implicit conv : C => IRCompilation[D, H])
  : ExplicitNamedVarsOps[D, H] = new ExplicitNamedVarsOps[D, H](c)
  implicit def evViaPortConnection[D <: DFDesign, H <: shapeless.HList, C](c : C)(implicit conv : C => IRCompilation[D, H])
  : ViaPortConnectionOps[D, H] = new ViaPortConnectionOps[D, H](c)
  implicit def evFlatten[D <: DFDesign, H <: shapeless.HList, C](c : C)(implicit conv : C => IRCompilation[D, H])
  : FlattenOps[D, H] = new FlattenOps[D, H](c)
  implicit def evCalculator[D <: DFDesign, H <: shapeless.HList, C](c : C)(implicit conv : C => IRCompilation[D, H])
  : CalculatorOps[D, H] = new CalculatorOps[D, H](c)
  implicit def evSingleStepPrev[D <: DFDesign, H <: shapeless.HList, C](c : C)(implicit conv : C => IRCompilation[D, H])
  : SingleStepPrevOps[D, H] = new SingleStepPrevOps[D, H](c)
  
  trait PreCompiler[D <: DFDesign, H <: shapeless.HList, H2 <: shapeless.HList] {
    def apply(fromStage : IRCompilation[D, H]) : IRCompilation[D, H2]
  }
  object PreCompiler {
    implicit def defaultDoesNothing[D <: DFDesign, H <: shapeless.HList] : PreCompiler[D, H, H] =
      (fromStage : IRCompilation[D, H]) => fromStage
  }
}
