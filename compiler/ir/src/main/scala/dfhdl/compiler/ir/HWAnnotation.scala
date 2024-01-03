package dfhdl.compiler.ir

import dfhdl.compiler.printing.Printer
import scala.annotation.StaticAnnotation
import dfhdl.internals.HasTypeName
import scala.annotation.Annotation

abstract class HWAnnotation extends StaticAnnotation, HasTypeName, Product, Serializable:
  val isActive: Boolean
  def codeString(using Printer): String = this.typeName

given CanEqual[HWAnnotation, HWAnnotation] = CanEqual.derived

extension (annotList: List[Annotation])
  def getActiveHWAnnotations: List[HWAnnotation] = annotList.collect {
    case annot: HWAnnotation if annot.isActive => annot
  }
