package ZFiant

package object compiler {
  implicit def evFixAnonymous[D <: DFDesign, S <: shapeless.HList, C](c : C)(implicit conv : C => Compilable[D, S])
  : FixAnonymousOps[D, S] = new FixAnonymousOps[D, S](c)
  implicit def evExplicitPrev[D <: DFDesign, S <: shapeless.HList, C](c : C)(implicit conv : C => Compilable[D, S]) 
  : ExplicitPrevOps[D, S] = new ExplicitPrevOps[D, S](c)
  implicit def evViaPortConnection[D <: DFDesign, S <: shapeless.HList, C](c : C)(implicit conv : C => Compilable[D, S])
  : ViaPortConnectionOps[D, S] = new ViaPortConnectionOps[D, S](c)
  implicit def evFlatten[D <: DFDesign, S <: shapeless.HList, C](c : C)(implicit conv : C => Compilable[D, S])
  : FlattenOps[D, S] = new FlattenOps[D, S](c)
  implicit def evCalculator[D <: DFDesign, S <: shapeless.HList, C](c : C)(implicit conv : C => Compilable[D, S])
  : CalculatorOps[D, S] = new CalculatorOps[D, S](c)
}
