package DFiant.compiler
package printing
import ir.*
import DFiant.core.DFC

trait Printer extends DFTypePrinter, DFTokenPrinter

object DefaultPrinter extends Printer
trait CPrinter extends Printer:
  val ctx: DFC
