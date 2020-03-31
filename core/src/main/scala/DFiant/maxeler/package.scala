package DFiant

import compiler.Compilable

package object maxeler {
  implicit def evMaxJNodeOps[D <: DFDesign, S <: shapeless.HList, C](c : C)(implicit conv : C => Compilable[D, S])
  : MaxJNodeOps[D, S] = new MaxJNodeOps[D, S](c)
}
