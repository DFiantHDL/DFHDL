package DFiant.core

import DFiant.internals._

trait DFDesign {
  protected implicit val protDesign = this
  protected[DFiant] val protAlmanac = new Almanac {}
}
object DFDesign
