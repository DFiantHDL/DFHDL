package dfhdl.core
import dfhdl.internals.*
import dfhdl.compiler.ir
import dfhdl.options.ElaborationOptions
import dfhdl.hw.annotation.getActiveHWAnnotations
import scala.reflect.ClassTag
import collection.mutable
import scala.annotation.Annotation
import scala.annotation.implicitNotFound
import ir.annotation.HWAnnotation

final case class DFC(
    nameOpt: Option[String],
    position: Position,
    docOpt: Option[String],
    annotations: List[HWAnnotation] = Nil, // TODO: removing default causes stale symbol crash
    mutableDB: MutableDB = new MutableDB(),
    refGen: ir.RefGen = new ir.RefGen((0, 0), 0),
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
    if (refGen.getGrpId == (0, 0))
      refGen.setGrpId(DFC.getGrpId(position))
    copy(
      nameOpt = nameOpt,
      position = position,
      docOpt = docOpt,
      annotations = annotations.getActiveHWAnnotations
    ).asInstanceOf[this.type]
  def setMeta(
      meta: ir.Meta
  ) =
    if (refGen.getGrpId == (0, 0))
      refGen.setGrpId(DFC.getGrpId(position))
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
  import java.util.concurrent.atomic.AtomicInteger

  /** Thread-safe cache for generating unique group IDs based on position hash codes.
    *
    * Thread Safety Guarantees:
    *   - Uses `TrieMap` for thread-safe concurrent access to the cache
    *   - Each hash code gets its own `AtomicInteger` counter for unique ID generation
    *   - `getOrElseUpdate` atomically checks and creates new counters if needed
    *   - `AtomicInteger.getAndIncrement()` provides atomic increment operations
    *
    * This design ensures that:
    *   1. Multiple threads can safely access the cache concurrently
    *   2. Each position hash code gets a unique incremental ID
    *   3. No race conditions occur during counter creation or increment
    *   4. Memory usage is bounded by the number of unique position hash codes
    */
  private val positionCache = collection.concurrent.TrieMap.empty[Int, AtomicInteger]

  /** Generates a unique group ID tuple for a given position.
    *
    * The tuple consists of:
    *   - First element: The position's hash code (for grouping similar positions)
    *   - Second element: A unique incremental ID for positions with the same hash code
    *
    * Thread Safety:
    *   - This method is thread-safe and can be called concurrently by multiple threads
    *   - Uses atomic operations to ensure unique ID generation without race conditions
    *   - Each position hash code gets its own counter, preventing ID conflicts
    *
    * @param position
    *   The position to generate a group ID for
    * @return
    *   A tuple (hashCode, uniqueId) where uniqueId is guaranteed to be unique for this position
    */
  private def getGrpId(position: Position): (Int, Int) =
    val hashCode = position.hashCode()
    val counter = positionCache.getOrElseUpdate(hashCode, new AtomicInteger(0))
    (hashCode, counter.getAndIncrement())

  // DFC given must be inline to force new DFC is generated for every missing DFC summon.
  inline given dfc: DFC = emptyNoEO // (using TopLevel)
  def empty(eo: ElaborationOptions): DFC =
    DFC(None, Position.unknown, None, elaborationOptionsContr = () => eo)
  def emptyNoEO: DFC = DFC(None, Position.unknown, None)
  def global: DFC = DFC(None, Position.unknown, None)
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
