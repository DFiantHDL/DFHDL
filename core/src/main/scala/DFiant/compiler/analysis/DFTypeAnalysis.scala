package DFiant.compiler
package analysis
import DFiant.internals.*
import ir.*

object DFUInt:
  def unapply(arg: DFDecimal): Option[Int] =
    arg match
      case DFDecimal(false, width, 0) => Some(width)
      case _                          => None

object DFSInt:
  def unapply(arg: DFDecimal): Option[Int] =
    arg match
      case DFDecimal(true, width, 0) => Some(width)
      case _                         => None
