package ZFiant
package compiler

package object sync {
  implicit def evClockedPrevOps[D <: DFDesign, S <: shapeless.HList, C](c : C)(implicit conv : C => Compilable[D, S])
  : ClockedPrevOps[D, S] = new ClockedPrevOps[D, S](c)

}
