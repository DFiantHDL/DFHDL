package DFiant
package compiler.printing

import core.Context

trait Printer
object DefaultPrinter extends Printer
trait CPrinter extends Printer:
  val ctx: Context
