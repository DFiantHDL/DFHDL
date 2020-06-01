package DFiant
package sim.ghdl

import DFDesign.DB.Patch
import compiler.Compilable
import compiler.backend.vhdl.VHDLCompiler
import shapeless.{:: => #:}

final class GHDLOps[D <: DFDesign, S <: shapeless.HList](c : Compilable[D, VHDLCompiler #: S]) {
  private val designDB = c.db
  import designDB.__getset
  def simulate = {
    c.newStage[GHDL](designDB, Seq())
  }
}

trait GHDL extends Compilable.Stage