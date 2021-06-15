package DFiant.compiler
package printing
import ir.*
import DFiant.core.Context

trait Printer extends DFTypePrinter, DFTokenPrinter

object DefaultPrinter extends Printer
trait CPrinter extends Printer:
  val ctx: Context
