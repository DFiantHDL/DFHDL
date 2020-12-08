package DFiant
package compiler

import DFiant.compiler.csprinter.CSPrinter

object CyclicEvaluator extends ElementEvaluator[Boolean] {
  def applyElement(cyclic: Boolean, element: SourceElement): Boolean = {
    cyclic
  }

  def apply(element: SourceElement)(implicit
      evaluationMap: EvaluationMap[Boolean],
      dependencyContext: DependencyContext
  ): Boolean = false

  def cyclic(element: SourceElement)(implicit
      evaluationMap: EvaluationMap[Boolean],
      dependencyContext: DependencyContext
  ): Boolean = true

  def codeString(t: Boolean)(implicit printer: CSPrinter): String = t.toString
}
