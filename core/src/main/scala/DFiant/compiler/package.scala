package DFiant

package object compiler {
  implicit def evFixAnonymous[D <: DFDesign, S <: shapeless.HList, C](c : C)(implicit conv : C => Compilable[D, S])
  : FixAnonymousOps[D, S] = new FixAnonymousOps[D, S](c)
  implicit def evUniqueDesigns[D <: DFDesign, S <: shapeless.HList, C](c : C)(implicit conv : C => Compilable[D, S])
  : UniqueDesignsOps[D, S] = new UniqueDesignsOps[D, S](c)
  implicit def evUniqueNames[D <: DFDesign, S <: shapeless.HList, C](c : C)(implicit conv : C => Compilable[D, S])
  : UniqueNamesOps[D, S] = new UniqueNamesOps[D, S](c)
  implicit def evRemovePureInterfaces[D <: DFDesign, S <: shapeless.HList, C](c : C)(implicit conv : C => Compilable[D, S])
  : RemovePureInterfacesOps[D, S] = new RemovePureInterfacesOps[D, S](c)
  implicit def evExplicitPrev[D <: DFDesign, S <: shapeless.HList, C](c : C)(implicit conv : C => Compilable[D, S]) 
  : ExplicitPrevOps[D, S] = new ExplicitPrevOps[D, S](c)
  implicit def evExplicitConversions[D <: DFDesign, S <: shapeless.HList, C](c : C)(implicit conv : C => Compilable[D, S])
  : ExplicitConversionsOps[D, S] = new ExplicitConversionsOps[D, S](c)
  implicit def evExplicitNamedVars[D <: DFDesign, S <: shapeless.HList, C](c : C)(implicit conv : C => Compilable[D, S])
  : ExplicitNamedVarsOps[D, S] = new ExplicitNamedVarsOps[D, S](c)
  implicit def evViaPortConnection[D <: DFDesign, S <: shapeless.HList, C](c : C)(implicit conv : C => Compilable[D, S])
  : ViaPortConnectionOps[D, S] = new ViaPortConnectionOps[D, S](c)
  implicit def evFlatten[D <: DFDesign, S <: shapeless.HList, C](c : C)(implicit conv : C => Compilable[D, S])
  : FlattenOps[D, S] = new FlattenOps[D, S](c)
  implicit def evCalculator[D <: DFDesign, S <: shapeless.HList, C](c : C)(implicit conv : C => Compilable[D, S])
  : CalculatorOps[D, S] = new CalculatorOps[D, S](c)
  implicit def evSingleStepPrev[D <: DFDesign, S <: shapeless.HList, C](c : C)(implicit conv : C => Compilable[D, S])
  : SingleStepPrevOps[D, S] = new SingleStepPrevOps[D, S](c)
}
