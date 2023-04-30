package dfhdl.compiler.ir
import dfhdl.internals.*

final case class Meta(
    nameOpt: Option[String],
    position: Position,
    docOpt: Option[String]
):
  val isAnonymous: Boolean = nameOpt.isEmpty
  val name: String =
    nameOpt.getOrElse(s"anon${this.hashCode.toHexString}")
  val comment: String = docOpt.getOrElse("")
  def anonymize: Meta = copy(nameOpt = None, docOpt = None)
  def setName(name: String): Meta = copy(nameOpt = Some(name))
  def setDoc(doc: String): Meta = copy(docOpt = Some(doc))
  def =~(that: Meta): Boolean = this.nameOpt == that.nameOpt && this.docOpt == that.docOpt
