package dfhdl.core
import dfhdl.internals.*
import dfhdl.compiler.ir
import dfhdl.options.ElaborationOptions
import dfhdl.hw.{HWAnnotation, getActiveHWAnnotations}
import scala.reflect.ClassTag
import collection.mutable
import scala.annotation.Annotation
import scala.annotation.implicitNotFound

final case class DFC(
    nameOpt: Option[String],
    position: Position,
    docOpt: Option[String],
    annotations: List[HWAnnotation] = Nil, // TODO: removing default causes stale symbol crash
    mutableDB: MutableDB = new MutableDB(),
    refGen: ir.RefGen = new ir.RefGen(0, 0),
    tags: ir.DFTags = ir.DFTags.empty,
    elaborationOptionsContr: () => ElaborationOptions = () =>
      summon[ElaborationOptions.Defaults[Design]]
) extends MetaContext:
  lazy val elaborationOptions: ElaborationOptions = elaborationOptionsContr()
  def setMeta(
      nameOpt: Option[String] = nameOpt,
      position: Position = position,
      docOpt: Option[String] = docOpt,
      annotations: List[Annotation] = Nil
  ) =
    if (refGen.getGrpId == 0)
      refGen.setGrpId(position.hashCode())
    copy(
      nameOpt = nameOpt,
      position = position,
      docOpt = docOpt,
      annotations = annotations.getActiveHWAnnotations
    ).asInstanceOf[this.type]
  def setMeta(
      meta: ir.Meta
  ) =
    if (refGen.getGrpId == 0)
      refGen.setGrpId(position.hashCode())
    copy(
      nameOpt = meta.nameOpt,
      position = meta.position,
      docOpt = meta.docOpt,
      annotations = meta.annotations
    ).asInstanceOf[this.type]
  def setTags(tags: ir.DFTags) = copy(tags = tags)
  def tag[CT <: ir.DFTag: ClassTag](customTag: CT) = setTags(tags.tag(customTag))
  def emptyTags = setTags(ir.DFTags.empty)
  given getSet: ir.MemberGetSet = mutableDB.getSet
  def getMeta: ir.Meta = ir.Meta(nameOpt, position, docOpt, annotations)
  def enterOwner(owner: DFOwnerAny): Unit =
    mutableDB.OwnershipContext.enter(owner.asIR)
  def exitOwner(): Unit = mutableDB.OwnershipContext.exit()
  def owner: DFOwnerAny = mutableDB.OwnershipContext.owner.asFE
  def enterLate(): Unit =
    mutableDB.OwnershipContext.enterLate()
  def exitLate(): Unit =
    mutableDB.OwnershipContext.exitLate()
  def lateConstruction: Boolean = mutableDB.OwnershipContext.lateConstruction
  def ownerOption: Option[DFOwnerAny] =
    mutableDB.OwnershipContext.ownerOption.map(_.asFE)
  def ownerOrEmptyRef: ir.DFOwner.Ref =
    ownerOption.map(_.asIR.ref(using this)).getOrElse(ir.DFMember.Empty.ref(using this))
  def setName(name: String): this.type =
    copy(nameOpt = Some(name)).asInstanceOf[this.type]
  def setAnnotations(annotations: List[HWAnnotation]): this.type =
    copy(annotations = annotations).asInstanceOf[this.type]
  def anonymize: this.type = copy(nameOpt = None).asInstanceOf[this.type]
  def logError(err: DFError): Unit = mutableDB.logger.logError(err)
  def getErrors: List[DFError] = mutableDB.logger.getErrors
  def inMetaProgramming: Boolean = mutableDB.inMetaProgramming
  def clearErrors(): Unit = mutableDB.logger.clearErrors()
end DFC
object DFC:
  // DFC given must be inline to force new DFC is generated for every missing DFC summon.
  inline given dfc: DFC = emptyNoEO // (using TopLevel)
  def empty(eo: ElaborationOptions): DFC =
    DFC(None, Position.unknown, None, elaborationOptionsContr = () => eo)
  def emptyNoEO: DFC = DFC(None, Position.unknown, None)
  sealed trait Scope
  object Scope:
    sealed trait Global extends Scope
    object Global extends Global
    given Global = Global
    sealed trait Local extends Scope
    sealed trait Design extends Local
    object Design extends Design
    sealed trait Domain extends Local
    object Domain extends Domain
    sealed trait Process extends Local:
      // will include the step cache according to the name of the step block
      // (the plugin will make sure that the name is unique)
      private[core] val stepCache = mutable.Map.empty[String, ir.StepBlock]
    object Process extends Process
    sealed trait Interface extends Local
    object Interface extends Interface
  end Scope
end DFC

transparent inline def dfc(using d: DFC): d.type = d

trait HasDFC:
  lazy val dfc: DFC
  protected given DFC = dfc
