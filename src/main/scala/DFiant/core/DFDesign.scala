package DFiant.core

import DFiant.internals._

trait DFDesign {
  implicit val dsn = this
  val almanac = new Almanac {}
}
object DFDesign

object GlobalDesign extends DFDesign