package dfhdl.compiler.ir
import dfhdl.internals.*
import dfhdl.hw.{HWAnnotation, getActiveHWAnnotations}

import scala.annotation.Annotation

final case class Meta(
    nameOpt: Option[String],
    position: Position,
    docOpt: Option[String],
    annotations: List[HWAnnotation]
) derives CanEqual:
  val isAnonymous: Boolean = nameOpt.isEmpty
  val name: String =
    nameOpt.getOrElse(s"anon${this.hashString}")
  val comment: String = docOpt.getOrElse("")
  def anonymize: Meta = copy(nameOpt = None, docOpt = None)
  def setName(name: String): Meta = copy(nameOpt = Some(name))
  def setDoc(doc: String): Meta = copy(docOpt = Some(doc))
  def setAnnotations(annotations: List[HWAnnotation]) = copy(annotations = annotations)
  def addAnnotation(annotation: HWAnnotation) = setAnnotations(annotation :: annotations)
  def removeAnnotation(annotation: HWAnnotation) = setAnnotations(
    annotations.filterNot(_ == annotation)
  )
  def =~(that: Meta): Boolean =
    this.nameOpt == that.nameOpt && this.docOpt == that.docOpt && this.annotations == that.annotations
end Meta

object Meta:
  def empty: Meta = Meta(None, Position.unknown, None, Nil)
  def gen(
      nameOpt: Option[String],
      position: Position,
      docOpt: Option[String],
      annotations: List[Annotation]
  ): Meta = Meta(nameOpt, position, docOpt, annotations.getActiveHWAnnotations)
