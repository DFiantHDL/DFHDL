package DFiant
package core
import internals.*
import compiler.ir

final case class DFC(
    nameOpt: Option[String],
    position: Position,
    lateConstruction: Boolean,
    clsNameOpt: Option[String],
    clsPosition: Position,
    defaultDir: Int = 0
) extends MetaContext:
  def setMeta(
      nameOpt: Option[String],
      position: Position,
      lateConstruction: Boolean,
      clsNameOpt: Option[String],
      clsPosition: Position
  ) = copy(
    nameOpt = nameOpt,
    position = position,
    lateConstruction = lateConstruction,
    clsNameOpt = clsNameOpt,
    clsPosition = clsPosition
  ).asInstanceOf[this.type]
  def getOwnerMeta : ir.OwnerMeta = ???
  def getMemberMeta : ir.MemberMeta = ???
  val mutableDB: MutableDB = new MutableDB()
  def enterOwner(owner: DFOwner): Unit =
    mutableDB.OwnershipContext.enter(owner.asIR)
  def exitOwner(): Unit = mutableDB.OwnershipContext.exit()
  def owner: DFOwner = mutableDB.OwnershipContext.owner.asFE
  def ownerOption: Option[DFOwner] =
    mutableDB.OwnershipContext.ownerOption.map(_.asFE)
  def setName(name: String): this.type =
    copy(nameOpt = Some(name)).asInstanceOf[this.type]
  def anonymize: this.type = copy(nameOpt = None).asInstanceOf[this.type]
  def <>(that: Int): this.type = copy(defaultDir = that).asInstanceOf[this.type]

end DFC
object DFC:
  given empty(using TopLevel): DFC =
    DFC(None, Position.unknown, false, None, Position.unknown)

def dfc(using DFC): DFC = summon[DFC]
