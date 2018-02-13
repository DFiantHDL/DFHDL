package DFiant.core

import DFiant.internals._

trait DFDesign extends DFPortShare {
  protected implicit val protDesign = this
  protected[DFiant] val protAlmanac = new Almanac {}
  def compileToVHDL(fileName : String) = ???
}
object DFDesign
