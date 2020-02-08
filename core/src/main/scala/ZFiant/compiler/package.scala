package ZFiant

package object compiler {
//  implicit def evCalculator[C : Compilable](c : C) : Calculator[C] = new Calculator[C](c)

  implicit def evUtils[D <: DFDesign, S <: shapeless.HList, C](c : C)(implicit conv : C => Compilable[D, S])
  : UtilsOps[D, S] = new UtilsOps[D, S](c)
  implicit def evExplicitPrev[D <: DFDesign, S <: shapeless.HList, C](c : C)(implicit conv : C => Compilable[D, S]) 
  : ExplicitPrevOps[D, S] = new ExplicitPrevOps[D, S](c)
  implicit def evViaPortConnection[D <: DFDesign, S <: shapeless.HList, C](c : C)(implicit conv : C => Compilable[D, S])
  : ViaPortConnectionOps[D, S] = new ViaPortConnectionOps[D, S](c)

  //  implicit def evFlatten[C : Compilable](c : C) : Flatten[C] = new Flatten[C](c)
}
