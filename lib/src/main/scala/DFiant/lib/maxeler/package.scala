package DFiant
package lib

import DFiant.compiler.IRCompilation
import DFiant.compiler.backend.BackendStage
import shapeless.HList

package object maxeler {
  implicit object maxjCompiler extends BackendStage.Compiler[MaxJNode] {
    def apply[D <: DFDesign, S <: HList](c : IRCompilation[D, S])
    : BackendStage.Compilation[D, MaxJNode] = new MaxJNodeOps[D, S](c).maxjCompile
  }
}
