package DFiant
package lib

import DFiant.compiler.IRCompilation
import DFiant.compiler.backend.BackendStage

package object maxeler {
  implicit object maxjCompiler extends BackendStage.Compiler[MaxJNode] {
    def apply[D <: DFDesign](c : IRCompilation[D])
    : BackendStage.Compilation[D, MaxJNode] = new MaxJNodeOps[D](c).maxjCompile
  }
}
