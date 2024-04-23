/** this package contains all the dfhdl hardware annotations
  */
package dfhdl.hw

export dfhdl.compiler.ir.HWAnnotation

final case class unused(isActive: Boolean) extends HWAnnotation:
  def this() = this(true)
final case class pure(isActive: Boolean) extends HWAnnotation:
  def this() = this(true)
