package dfhdl.compiler.ir
import dfhdl.internals.*

import scala.annotation.Annotation

final case class Meta(
    nameOpt: Option[String],
    position: Position,
    docOpt: Option[String],
    annotations: List[Annotation]
) derives CanEqual:
  val isAnonymous: Boolean = nameOpt.isEmpty
  val name: String =
    nameOpt.getOrElse(s"anon${this.hashCode.toHexString}")
  val comment: String = docOpt.getOrElse("")
  def anonymize: Meta = copy(nameOpt = None, docOpt = None)
  def setName(name: String): Meta = copy(nameOpt = Some(name))
  def setDoc(doc: String): Meta = copy(docOpt = Some(doc))
  def setAnnotations(annotations: List[Annotation]) = copy(annotations = annotations)
  def addAnnotation(annotation: Annotation) = setAnnotations(annotation :: annotations)
  def removeAnnotation(annotation: Annotation) = setAnnotations(
    annotations.filterNot(_ == annotation)
  )
  def =~(that: Meta): Boolean =
    this.nameOpt == that.nameOpt && this.docOpt == that.docOpt && this.annotations == that.annotations
end Meta
