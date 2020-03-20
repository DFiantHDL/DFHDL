package ZFiant
package compiler.backend.vhdl

object PackageFile {
  def apply()(implicit printer: Printer) : String = {
    ""
  }
  def Name()(implicit printer: Printer) : String = s"${printer.getSet.designDB.top.designType}_pack"
}
