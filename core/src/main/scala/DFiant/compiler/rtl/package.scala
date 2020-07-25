package DFiant
package compiler

package object rtl {
  implicit def evClockedPrevOps[D <: DFDesign, C](c : C)(implicit conv : C => IRCompilation[D])
  : ClockedPrevOps[D] = new ClockedPrevOps[D](c)
}
