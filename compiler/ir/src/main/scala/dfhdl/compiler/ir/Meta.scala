package dfhdl.compiler.ir
import dfhdl.internals.*
import upickle.default.*
import annotation.HWAnnotation

final case class Meta(
    nameOpt: Option[String],
    position: Position,
    docOpt: Option[String],
    annotations: List[HWAnnotation]
) extends HasRefCompare[Meta] derives CanEqual, ReadWriter:
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
  protected def `prot_=~`(that: Meta)(using MemberGetSet): Boolean =
    this.nameOpt == that.nameOpt && this.docOpt == that.docOpt &&
      this.annotations.lazyZip(that.annotations).forall(_ =~ _)
  lazy val getRefs: List[DFRef.TwoWayAny] =
    annotations.flatMap(_.getRefs)
  def copyWithNewRefs(using RefGen): this.type = copy(
    annotations = annotations.map(_.copyWithNewRefs)
  ).asInstanceOf[this.type]
end Meta

object Meta:
  given ReadWriter[Position] = macroRW
  def empty: Meta = Meta(None, Position.unknown, None, Nil)
