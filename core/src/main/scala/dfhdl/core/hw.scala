package dfhdl.core

import dfhdl.compiler.ir.HWAnnotation
import dfhdl.compiler.printing.Printer

/** contains all the dfhdl hardware annotations
  */
object hw:

  final case class unused(isActive: Boolean) extends HWAnnotation:
    def this() = this(true)
  final case class pure(isActive: Boolean) extends HWAnnotation:
    def this() = this(true)
