package dfhdl.compiler.ir

import dfhdl.compiler.printing.Printer
import scala.annotation.StaticAnnotation
import dfhdl.internals.HasTypeName

abstract class HWAnnotation(when: Boolean) extends StaticAnnotation with HasTypeName:
  def isActive: Boolean = when
  def codeString(using Printer): String = this.typeName
