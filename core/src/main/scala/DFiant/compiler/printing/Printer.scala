package DFiant
package compiler.printing

import core.Context

trait Printer

trait CPrinter extends Printer:
  val ctx: Context
