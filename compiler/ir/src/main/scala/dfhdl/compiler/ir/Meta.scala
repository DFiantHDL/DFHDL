package dfhdl.compiler.ir
import dfhdl.internals.*
import upickle.default.*
import annotation.HWAnnotation

final case class Meta(
    nameOpt: Option[String],
    position: Position,
    docOpt: Option[String],
    annotations: List[HWAnnotation],
    // The Scala package path of the DECLARATION this meta describes ("" for the root
    // package and for members whose namespace is their design scope, i.e. regular
    // values). Captured by the plugin for design classes and methods and by the type
    // derivation macros for named DFTypes; the eventual packages feature places a
    // named type by relating its namespace to the top design's.
    namespace: String = ""
) extends HasRefCompare[Meta] derives ReadWriter:
  // Two distinct comparisons, and deliberately NO `CanEqual` (a direct `meta == meta`
  // does not compile under strictEquality), so every call site names which one it means:
  //
  // `sameIdentityAs`: excludes `position` and `docOpt`. The elaboration cache digest
  // hashes typed trees, so positions and doc comments can drift while a cached entry
  // stays valid, and cached members must still unify by value with their live
  // counterparts (globals in `SubDesignEntry.cloneForAdoption` and sub-DB assembly
  // unify by member equality). `namespace` is digest-visible (a package clause is in
  // the typed tree), so it participates. This is also what `equals`/`hashCode`
  // implement, since member case-class equality composes through them implicitly.
  //
  // `sameDclAs`: all fields, position and doc included. Answers "same declaration",
  // which position is what anchors — same-named designs from different declarations
  // must not unify (`DesignLoadKey`'s intra-run gate tier, `UniqueDesigns`' grouping).
  def sameIdentityAs(that: Meta): Boolean =
    this.nameOpt == that.nameOpt && this.namespace == that.namespace &&
      this.annotations == that.annotations
  def sameDclAs(that: Meta): Boolean =
    this.sameIdentityAs(that) && this.position == that.position && this.docOpt == that.docOpt
  override def equals(that: Any): Boolean = that match
    case that: Meta => this.sameIdentityAs(that)
    case _          => false
  override def hashCode: Int = (nameOpt, namespace, annotations).##
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
    this.nameOpt == that.nameOpt && this.namespace == that.namespace &&
      this.docOpt == that.docOpt &&
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
  // meta of a SYNTHESIZED named declaration (a compiler-made type/design with no
  // Scala declaration behind it): name only, unknown position, root namespace
  def named(name: String, namespace: String = ""): Meta =
    Meta(Some(name), Position.unknown, None, Nil, namespace)

  /** Fold a DFHDL class-inheritance chain (the plugin-injected `__clsMeta`, most-derived first)
    * into the leaf's meta: the leaf's name, position, doc and namespace, carrying the class
    * annotations of the WHOLE chain.
    *
    * A design/interface is emitted FLAT, so a base class has no construct of its own to hold an
    * annotation and its annotations must reach the leaf. They are merged rather than concatenated
    * because every consumer reads them with `collectFirst` (the resolved clk/rst timing,
    * `flattenMode`, the purity marking): two `@timing.clock`s in one list would mean the base's
    * fields are silently dropped instead of inherited. Same-kind signal constraints therefore merge
    * field by field with the more-derived class taking priority, so a base's `rate` survives a leaf
    * that sets only `edge`; annotation kinds that do not compose keep their most-derived occurrence
    * first, which is that same priority as `collectFirst` reads it.
    */
  def foldClsChain(chain: List[Meta]): Option[Meta] = chain match
    case Nil         => None
    case leaf :: Nil => Some(leaf)
    case leaf :: _   =>
      // most-derived first, so an already-accumulated annotation always outranks the incoming one
      val folded = chain.flatMap(_.annotations)
        .foldLeft(List.empty[HWAnnotation]) { (acc, base) =>
          var merged = false
          val updated = acc.map {
            case derived if merged                  => derived
            case derived: constraints.SigConstraint =>
              base match
                case baseSig: constraints.SigConstraint =>
                  baseSig.merge(derived, withPriority = true) match
                    case Some(mergedSig) =>
                      merged = true
                      mergedSig
                    case None => derived
                case _ => derived
            case derived =>
              if (derived == base) merged = true
              derived
          }
          if (merged) updated else acc :+ base
        }
      Some(leaf.setAnnotations(folded))
  end foldClsChain
end Meta
