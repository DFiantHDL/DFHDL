package DFiant

import compiler.backend._
package object compiler {
  implicit def evSanityCheck[D <: DFDesign, C](c : C)(implicit conv : C => IRCompilation[D])
  : SanityCheck[D] = new SanityCheck[D](c)
  implicit def evFixAnonymous[D <: DFDesign, C](c : C)(implicit conv : C => IRCompilation[D])
  : FixAnonymous[D] = new FixAnonymous[D](c)
  implicit def evUniqueDesigns[D <: DFDesign, C](c : C)(implicit conv : C => IRCompilation[D])
  : UniqueDesigns[D] = new UniqueDesigns[D](c)
  implicit def evNamedSelection[D <: DFDesign, C](c : C)(implicit conv : C => IRCompilation[D])
  : NamedSelection[D] = new NamedSelection[D](c)
  implicit def evUniqueNames[D <: DFDesign, C](c : C)(implicit conv : C => IRCompilation[D])
  : UniqueNames[D] = new UniqueNames[D](c)
  implicit def evFlattenNames[D <: DFDesign, C](c : C)(implicit conv : C => IRCompilation[D])
  : FlattenNames[D] = new FlattenNames[D](c)
  implicit def evExplicitPrev[D <: DFDesign, C](c : C)(implicit conv : C => IRCompilation[D])
  : ExplicitPrev[D] = new ExplicitPrev[D](c)
  implicit def evExplicitConversions[D <: DFDesign, C](c : C)(implicit conv : C => IRCompilation[D])
  : ExplicitConversions[D] = new ExplicitConversions[D](c)
  implicit def evExplicitNamedVars[D <: DFDesign, C](c : C)(implicit conv : C => IRCompilation[D])
  : ExplicitNamedVars[D] = new ExplicitNamedVars[D](c)
  implicit def evViaPortConnection[D <: DFDesign, C](c : C)(implicit conv : C => IRCompilation[D])
  : ViaPortConnection[D] = new ViaPortConnection[D](c)
  implicit def evFlatten[D <: DFDesign, C](c : C)(implicit conv : C => IRCompilation[D])
  : Flatten[D] = new Flatten[D](c)
  implicit def evCalculator[D <: DFDesign, C](c : C)(implicit conv : C => IRCompilation[D])
  : Calculator[D] = new Calculator[D](c)
  implicit def evSingleStepPrev[D <: DFDesign, C](c : C)(implicit conv : C => IRCompilation[D])
  : SingleStepPrev[D] = new SingleStepPrev[D](c)
  implicit def evRTL[D <: DFDesign, C](c : C)(implicit conv : C => IRCompilation[D])
  : RTL[D] = new RTL[D](c)

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
