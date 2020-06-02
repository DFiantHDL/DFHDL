package DFiant
package lib

import DFiant.compiler.IRCompilation
import DFiant.compiler.backend.Backend
import shapeless.HList

package object maxeler {
  implicit object maxjCompiler extends Backend.Compiler[MaxJNode] {
    def apply[D <: DFDesign, S <: HList](c : IRCompilation[D, S])
    : Backend.Compilation[D, MaxJNode] = new MaxJNodeOps[D, S](c).maxjCompile
  }
}
