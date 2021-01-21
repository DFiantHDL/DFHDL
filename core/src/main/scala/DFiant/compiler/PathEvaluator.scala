package DFiant
package compiler

import DFiant.compiler.DependencyContext.ConsumeRef
import DFiant.compiler.csprinter.CSPrinter

import scala.annotation.tailrec
import scala.collection.immutable.ListSet

trait ElementEvaluator[T] {self =>
  implicit val __this : ElementEvaluator[T] = this
  val verboseEvaluation : Boolean = false
  def applyElement(t : => T, element : SourceElement) : T
  protected implicit class TOps(t : T) {
    def applyElement(element : SourceElement) : T = self.applyElement(t, element)
  }
  protected def concat(dfType : DFAny.Type, elements : List[T]) : T

  final protected implicit class Evaluate(src : Source) {
    def evaluate(dfType : DFAny.Type)(
      implicit srcValEvaluation : SourceValueEvaluation[T], dependencyContext : DependencyContext
    ) : T = concat(dfType, src.elements.map(srcValEvaluation.evaluate))
  }

  final protected implicit class Evaluate2(ref : ConsumeRef) {
    def evaluate(
      implicit srcValEvaluation : SourceValueEvaluation[T], dependencyContext : DependencyContext
    ) : T = {
      import dependencyContext.getSet
      ref.getSource.evaluate(ref.get.dfType)
    }
  }
  final protected implicit class Evaluate3(ref : DFAny.Alias.RelValRef) {
    def evaluate(
      implicit srcValEvaluation : SourceValueEvaluation[T], dependencyContext : DependencyContext
    ) : T = {
      import dependencyContext.getSet
      ref.getSource.evaluate(ref.get.dfType)
    }
  }
  final protected implicit class Evaluate4(ref : DFAny.ApplySel.RelValRef) {
    def evaluate(
      implicit srcValEvaluation : SourceValueEvaluation[T], dependencyContext : DependencyContext
    ) : T = {
      import dependencyContext.getSet
      ref.getSource.evaluate(ref.get.dfType)
    }
  }
  final protected implicit class Evaluate5(dcl : DFAny.Dcl) {
    def evaluate(version : SourceVersion)(
      implicit srcValEvaluation : SourceValueEvaluation[T], dependencyContext : DependencyContext
    ) : T = srcValEvaluation.evaluate(SourceElement(dcl, version, withInit = false))
  }

  def apply(srcVal : SourceValue)(
    implicit srcValEvaluation : SourceValueEvaluation[T], dependencyContext: DependencyContext
  ) : T
  final def apply(element : SourceElement)(
    implicit srcValEvaluation : SourceValueEvaluation[T], dependencyContext: DependencyContext
  ) : T = applyElement(apply(element.srcVal), element)
  def cyclic(srcVal : SourceValue)(
    implicit srcValEvaluation : SourceValueEvaluation[T], dependencyContext: DependencyContext
  ) : T
  final def cyclic(element : SourceElement)(
    implicit srcValEvaluation : SourceValueEvaluation[T], dependencyContext: DependencyContext
  ) : T = applyElement(cyclic(element.srcVal), element)
  def initialMap : SourceValueEvaluation[T] = Map()
  def getDependencies(element : SourceElement)(
    implicit dependencyContext: DependencyContext
  ) : List[SourceElement] = element.getDependencies(DependencyKind.Data)
  def codeString(t : T)(implicit printer: CSPrinter) : String
}

final class PathEvaluator[D <: DFDesign](c: IRCompilation[D]) {
  private val designDB = c.db
  import designDB.__getset
  private implicit val dc: DependencyContext = designDB.dependencyContext
  import dc.dependencyMap

  @tailrec private def evaluate[T](
    stack : List[(List[SourceElement], PendingTable)],
    srcValEvaluation : SourceValueEvaluation[T]
  )(implicit elementEvaluator: ElementEvaluator[T]) : SourceValueEvaluation[T] =
    stack match {
      //already evaluated current element
      case (currentElement :: next, pendingTable) :: nextStack if srcValEvaluation.contains(currentElement.srcVal) =>
        if (elementEvaluator.verboseEvaluation)
          println(
            "already evaluated", currentElement.codeString,
            "with value", elementEvaluator.codeString(srcValEvaluation(currentElement.srcVal))
          )
        evaluate((next, pendingTable) :: nextStack, srcValEvaluation)

      //cyclic dependency found
      case (currentDependency :: next, pendingTable) :: nextStack
          if pendingTable.waitsFor(currentDependency) =>
        val evaluatedCyclicElements =
          stack.view
          //get a tuple of all pending heads, with an indication if they wait for the cyclic dependency
          //we just discovered
            .collect {
              case (pendingElement :: _, pd: PendingTable) =>
                (pendingElement, pd.waitsFor(currentDependency))
            }
            //take only elements that participate in this cyclic path
            .takeWhile(_._2)
            //mark as cyclic path
            .map {
              case (element, _) => (element.srcVal, elementEvaluator.cyclic(element)(srcValEvaluation, dc))
            }
        if (elementEvaluator.verboseEvaluation) println("cyclic dependency found", currentDependency.codeString)
        evaluate((next, pendingTable) :: nextStack, srcValEvaluation ++ evaluatedCyclicElements)

      //finished evaluating dependencies of a cyclic path
      case (Nil, _) :: (currentElement :: next, pendingTable) :: nextStack if srcValEvaluation.contains(currentElement.srcVal) =>
        if (elementEvaluator.verboseEvaluation) println("finished cyclic path at", currentElement.codeString)
        evaluate((next, pendingTable) :: nextStack, srcValEvaluation)

      //finished evaluating dependencies
      case (Nil, _) :: (currentElement :: next, pendingTable) :: nextStack =>
        if (elementEvaluator.verboseEvaluation)
          println("evaluating",currentElement.codeString)
        val evaluatedElement = elementEvaluator(currentElement)(srcValEvaluation, dc)
        evaluate((next, pendingTable) :: nextStack, srcValEvaluation.updated(currentElement.srcVal, evaluatedElement))

      //new element to be evaluated, so we first add dependencies to the stack to evaluate them
      case (currentElement :: _, pendingTable) :: _ =>
        val dependencies        = elementEvaluator.getDependencies(currentElement)
        val updatedPendingTable = pendingTable.addPending(currentElement)
        if (elementEvaluator.verboseEvaluation)
          println("waiting for dependencies for", currentElement.codeString, ":", dependencies.map(_.codeString))
        evaluate((dependencies, updatedPendingTable) :: stack, srcValEvaluation)
      case _ => srcValEvaluation
    }

  def evaluate[T](elementEvaluator: ElementEvaluator[T]) : SourceValueEvaluation[T] = {
    evaluate(
      (
        dependencyMap.values.flatMap(_.elements).toList,
        PendingTable.empty
      ) :: Nil,
      elementEvaluator.initialMap
    )(elementEvaluator)
  }
  def printEvaluation[T](implicit elementEvaluator: ElementEvaluator[T]) : IRCompilation[D] = {
    val srcValEvaluation = evaluate(elementEvaluator)
    import CSPrinter.defaultPrinter
    println(ListSet.from(dependencyMap.values.flatMap(_.elements)).map(src =>
      f"${src.codeString}%-40s ${elementEvaluator.codeString(srcValEvaluation.evaluate(src))}"
    ).mkString("\n"))
    c
  }
}
