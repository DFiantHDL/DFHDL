package dfhdl.core

import dfhdl.compiler.ir.HWAnnotation
import dfhdl.compiler.printing.Printer

/** contains all the dfhdl hardware annotations
  */
object hw:

  class unused(when: Boolean) extends HWAnnotation(when):
    def this() = this(true)
