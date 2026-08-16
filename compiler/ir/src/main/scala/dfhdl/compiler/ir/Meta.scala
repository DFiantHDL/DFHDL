package dfhdl.compiler.ir
import dfhdl.internals.*
import upickle.default.*
import annotation.HWAnnotation

final case class Meta(
    nameOpt: Option[String],
    position: Position,
    docOpt: Option[String],
    annotations: List[HWAnnotation]
) extends HasRefCompare[Meta] derives ReadWriter:
  // Two distinct comparisons, and deliberately NO `CanEqual` (a direct `meta == meta`
  // does not compile under strictEquality), so every call site names which one it means:
  //
  // `sameIdentityAs`: excludes `position` and `docOpt`. The elaboration cache digest
  // hashes typed trees, so positions and doc comments can drift while a cached entry
  // stays valid, and cached members must still unify by value with their live
  // counterparts (globals in `SubDesignEntry.cloneForAdoption` and sub-DB assembly
  // unify by member equality). This is also what `equals`/`hashCode` implement, since
  // member case-class equality composes through them implicitly.
  //
  // `sameDclAs`: all fields, position and doc included. Answers "same declaration",
  // which position is what anchors — same-named designs from different declarations
  // must not unify (`DesignLoadKey`'s intra-run gate tier, `UniqueDesigns`' grouping).
  def sameIdentityAs(that: Meta): Boolean =
    this.nameOpt == that.nameOpt && this.annotations == that.annotations
  def sameDclAs(that: Meta): Boolean =
    this.sameIdentityAs(that) && this.position == that.position && this.docOpt == that.docOpt
  override def equals(that: Any): Boolean = that match
    case that: Meta => this.sameIdentityAs(that)
    case _          => false
  override def hashCode: Int = (nameOpt, annotations).##
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
