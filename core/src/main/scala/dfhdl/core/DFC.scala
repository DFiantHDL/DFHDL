package dfhdl
package core
import internals.*
import compiler.ir

final case class DFC(
    nameOpt: Option[String],
    position: Position,
    docOpt: Option[String],
    mutableDB: MutableDB = new MutableDB(),
    defaultDir: Int = 0
) extends MetaContext:
  def setMeta(
      nameOpt: Option[String] = nameOpt,
      position: Position = position,
      docOpt: Option[String] = docOpt
  ) = copy(
    nameOpt = nameOpt,
    position = position,
    docOpt = docOpt
  ).asInstanceOf[this.type]
  given getSet: ir.MemberGetSet = mutableDB.getSet
  def getMeta: ir.Meta = ir.Meta(nameOpt, position, docOpt)
  def enterOwner(owner: DFOwnerAny): Unit =
    mutableDB.OwnershipContext.enter(owner.asIR)
  def enterLate(): Unit =
    mutableDB.OwnershipContext.enterLate()
  def exitOwner(): Unit = mutableDB.OwnershipContext.exit()
  def owner: DFOwnerAny = mutableDB.OwnershipContext.owner.asFE
  def lateConstruction: Boolean = mutableDB.OwnershipContext.lateConstruction
  def ownerOption: Option[DFOwnerAny] =
    mutableDB.OwnershipContext.ownerOption.map(_.asFE)
  def setName(name: String): this.type =
    copy(nameOpt = Some(name)).asInstanceOf[this.type]
  def anonymize: this.type = copy(nameOpt = None).asInstanceOf[this.type]
  def <>(that: Int): this.type = copy(defaultDir = that).asInstanceOf[this.type]
  def logError(err: DFError): Unit = mutableDB.logger.logError(err)
  def getErrors: List[DFError] = mutableDB.logger.getErrors
  def clearErrors(): Unit = mutableDB.logger.clearErrors()
end DFC
object DFC:
  given (using TopLevel): DFC = empty
  def empty: DFC =
    DFC(None, Position.unknown, None)
  sealed trait Scope
  object Scope:
    sealed trait Design extends Scope
    object Design extends Design
    given Design = Design
    sealed trait Domain extends Scope
    object Domain extends Domain
    sealed trait Process extends Scope
    object Process extends Process
    sealed trait Interface extends Scope
    object Interface extends Interface
  sealed trait Domain
  object Domain:
    sealed trait DF extends Domain
    object DF extends DF
    given DF = DF
    sealed trait RT extends Domain
    object RT extends RT
    sealed trait ED extends Domain
    object ED extends ED
end DFC

def dfc(using DFC): DFC = summon[DFC]

trait HasDFC:
  val dfc: DFC
  protected given DFC = dfc
