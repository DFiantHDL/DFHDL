package DFiant.internals

trait Nameable {
  protected def nameDefault : String = "???"
  private var nameManual : String = ""
  private var nameAuto : String = ""
  def hasName : Boolean = !nameManual.isEmpty || !nameAuto.isEmpty
  lazy val name : String = if (!nameManual.isEmpty) nameManual
                           else if (!nameAuto.isEmpty) nameAuto
                           else nameDefault
  def setName(name : String) : this.type = {nameManual = name; this}
  final protected[DFiant] def setAutoName(name : String) : this.type = {nameAuto = name; this}
  override def toString : String = name
}

trait TypeNameable {
  private var typeNameAuto : String = "???"
  lazy val typeName : String = typeNameAuto
  final protected[DFiant] def setAutoTypeName(name : String) : this.type = {typeNameAuto = name; this}
}


trait NameIt {
  val isAnonymous : Boolean
  val value : String
}
object NameIt {
  implicit def ev(implicit name : sourcecode.Name, ownerName : sourcecode.OwnerName) : NameIt = new NameIt {
    val isAnonymous : Boolean = name.value == "$anon"
    val value: String = if (isAnonymous) ownerName.value else name.value
  }
}