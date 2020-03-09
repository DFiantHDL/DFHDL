package ZFiant
package compiler
package backend
package vhdl

import backend.utils._

final class Compiled(designDB : DFDesign.DB, block : DFDesign.Block) {
  import designDB.__getset

}

final class Compiler[D <: DFDesign, S <: shapeless.HList](c : Compilable[D, S]) {
  private val designDB = c.explicitPrev.viaPortConnection.uniqueDesigns.uniqueNames(reservedKeywords, caseSensitive = false).db

//  def testVHDL : Compiled = new Compiled(designDB, designDB.top)
}
