package dfhdl.compiler.ir

import dfhdl.compiler.printing.Printer
import scala.annotation.StaticAnnotation
import dfhdl.internals.HasTypeName
import scala.annotation.Annotation

abstract class HWAnnotation(when: Boolean) extends StaticAnnotation with HasTypeName:
  def isActive: Boolean = when
  def codeString(using Printer): String = this.typeName

given CanEqual[HWAnnotation, HWAnnotation] = CanEqual.derived

extension (annotList: List[Annotation])
  def getActiveHWAnnotations: List[HWAnnotation] = annotList.collect {
    case annot: HWAnnotation if annot.isActive => annot
  }
