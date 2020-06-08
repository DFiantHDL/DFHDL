package DFiant.compiler.backend.verilog

import DFiant.compiler.backend.Backend

sealed trait Revision extends Product with Serializable
object Revision {
  implicit case object V95 extends Revision
  type V95 = V95.type
  implicit case object V2005 extends Revision
  type V2005 = V2005.type
}

sealed trait VerilogBackend[R <: Revision] extends Backend.Stage