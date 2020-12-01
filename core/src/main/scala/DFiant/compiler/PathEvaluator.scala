package DFiant
package compiler

import DFiant.compiler.csprinter.CSPrinter

import scala.annotation.tailrec
import scala.collection.immutable.ListSet

trait ElementEvaluator[T] {self =>
  implicit val __this : ElementEvaluator[T] = this
  val verboseEvaluation : Boolean = false
  def applyElement(t : T, element : SourceElement) : T
  protected implicit class TOps(t : T) {
    def applyElement(element : SourceElement) : T = self.applyElement(t, element)
  }
  def apply(element : SourceElement)(
    implicit evaluationMap : EvaluationMap[T], dependencyContext: DependencyContext
  ) : T
  def cyclic(element : SourceElement)(
    implicit evaluationMap : EvaluationMap[T], dependencyContext: DependencyContext
  ) : T
  def initialMap : EvaluationMap[T] = Map()
  def getDependencies(element : SourceElement)(
    implicit dependencyContext: DependencyContext
  ) : List[SourceElement] = element.getDependencies(DependencyKind.Data)
  def codeString(t : T)(implicit printer: CSPrinter) : String
}

final class PathEvaluator[D <: DFDesign](c : IRCompilation[D]) {
  private val designDB = c.db
  import designDB.__getset
  private implicit val dc : DependencyContext = designDB.dependencyContext
  import dc.dependencyMap

  @tailrec private def evaluate[T](
    stack : List[(List[SourceElement], PendingTable)],
    evaluationMap : EvaluationMap[T]
  )(implicit elementEvaluator: ElementEvaluator[T]) : EvaluationMap[T] =
    stack match {
      //already evaluated current element
      case (currentElement :: next, pendingTable) :: nextStack if evaluationMap.contains(currentElement.srcVal) =>
        if (elementEvaluator.verboseEvaluation) println("already evaluated", currentElement.codeString)
        evaluate((next, pendingTable) :: nextStack, evaluationMap)

      //cyclic dependency found
      case (currentDependency :: next, pendingTable) :: nextStack if pendingTable.waitsFor(currentDependency) =>
        val evaluatedCyclicElements =
          stack.view
            //get a tuple of all pending heads, with an indication if they wait for the cyclic dependency
            //we just discovered
            .collect {
              case (pendingElement :: _, pd : PendingTable) => (pendingElement, pd.waitsFor(currentDependency))
            }
            //take only elements that participate in this cyclic path
            .takeWhile(_._2)
            //mark as cyclic path
            .map {
              case (element, _) => (element.srcVal, elementEvaluator.cyclic(element)(evaluationMap, dc))
            }
        if (elementEvaluator.verboseEvaluation) println("cyclic dependency found", currentDependency.codeString)
        evaluate((next, pendingTable) :: nextStack, evaluationMap ++ evaluatedCyclicElements)

      //finished evaluating dependencies of a cyclic path
      case (Nil, _) :: (currentElement :: next, pendingTable) :: nextStack if evaluationMap.contains(currentElement.srcVal) =>
        if (elementEvaluator.verboseEvaluation) println("finished cyclic path at", currentElement.codeString)
        evaluate((next, pendingTable) :: nextStack, evaluationMap)

      //finished evaluating dependencies
      case (Nil, _) :: (currentElement :: next, pendingTable) :: nextStack =>
        if (elementEvaluator.verboseEvaluation)
          println("evaluating",currentElement.codeString)
        val evaluatedElement = elementEvaluator(currentElement)(evaluationMap, dc)
        evaluate((next, pendingTable) :: nextStack, evaluationMap.updated(currentElement.srcVal, evaluatedElement))

      //new element to be evaluated, so we first add dependencies to the stack to evaluate them
      case (currentElement :: _, pendingTable) :: _ =>
        val dependencies = elementEvaluator.getDependencies(currentElement)
        val updatedPendingTable = pendingTable.addPending(currentElement)
        if (elementEvaluator.verboseEvaluation)
          println("waiting for dependencies for", currentElement.codeString, ":", dependencies.map(_.codeString))
        evaluate((dependencies, updatedPendingTable) :: stack, evaluationMap)
      case _ => evaluationMap
    }

  def evaluate[T](elementEvaluator: ElementEvaluator[T]) : EvaluationMap[T] = {
    evaluate(
      (dependencyMap.values.flatMap(_.elements).toList, PendingTable.empty) :: Nil,
      elementEvaluator.initialMap
    )(elementEvaluator)
  }
  def printEvaluation[T](implicit elementEvaluator: ElementEvaluator[T]) : IRCompilation[D] = {
    val evaluationMap = evaluate(elementEvaluator)
    import CSPrinter.defaultPrinter
    println(ListSet.from(dependencyMap.values.flatMap(_.elements)).map(src =>
      f"${src.codeString}%-40s ${elementEvaluator.codeString(evaluationMap.evaluate(src))}"
    ).mkString("\n"))
    c
  }
}