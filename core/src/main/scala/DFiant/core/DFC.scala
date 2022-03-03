package DFiant
package core
import internals.*
import compiler.ir

final case class DFC(
    nameOpt: Option[String],
    position: Position,
    mutableDB: MutableDB = new MutableDB(),
    defaultDir: Int = 0
) extends MetaContext:
  def setMeta(
      nameOpt: Option[String] = nameOpt,
      position: Position = position
  ) = copy(
    nameOpt = nameOpt,
    position = position
  ).asInstanceOf[this.type]
  given getSet: ir.MemberGetSet = mutableDB.getSet
  def getMeta: ir.Meta = ir.Meta(nameOpt, position)
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
    DFC(None, Position.unknown)

def dfc(using DFC): DFC = summon[DFC]

trait HasDFC:
  val dfc: DFC
  protected given DFC = dfc
