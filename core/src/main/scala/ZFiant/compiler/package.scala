package ZFiant

package object compiler {
  implicit def evUtils[C : Compilable](c : C) : Utils[C] = new Utils[C](c)
  implicit def evCalculator[C : Compilable](c : C) : Calculator[C] = new Calculator[C](c)
  implicit def evViaPortConnection[C : Compilable](c : C) : ViaPortConnection[C] = new ViaPortConnection[C](c)
  implicit def evExplicitPrev[C : Compilable](c : C) : ExplicitPrev[C] = new ExplicitPrev[C](c)
  implicit def evFlatten[C : Compilable](c : C) : Flatten[C] = new Flatten[C](c)
}
