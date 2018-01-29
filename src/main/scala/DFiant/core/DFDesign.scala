package DFiant.core

import DFiant.internals._

trait DFDesign {
  implicit val dsn = this
  val almanac = Almanac
}
object DFDesign

object GlobalDesign extends DFDesign