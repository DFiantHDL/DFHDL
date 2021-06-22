package DFiant.compiler
package printing
import ir.*
import DFiant.core.DFC

protected trait AbstractPrinter:
  val printer: Printer

trait Printer extends DFTypePrinter, DFTokenPrinter, DFValPrinter:
  val printer = this

object DefaultPrinter extends Printer
trait CPrinter extends Printer:
  val ctx: DFC
