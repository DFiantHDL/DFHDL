package DFiant.internals

trait Nameable {
  private[DFiant] def nameDefault : String = "???"
  private var nameManual : String = ""
  private var nameAuto : String = ""
  final def hasName : Boolean = !nameManual.isEmpty || !nameAuto.isEmpty
  final lazy val name : String = getUniqueName (
    if (!nameManual.isEmpty) nameManual
    else if (!nameAuto.isEmpty) nameAuto
    else nameDefault
  )
  private[internals] def getUniqueName(suggestedName : String) : String
  final def setName(name : String) : this.type = {nameManual = name; this}
  final protected[DFiant] def setAutoName(name : String) : this.type = {nameAuto = name; this}
  final protected[DFiant] def setAnonymous() : this.type = setAutoName("$anon")
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
    private val nameIsAnon = name.value == "$anon" || name.value == "$anonfun"
    private val ownerNameIsAnon = ownerName.value.contains('<')
    val isAnonymous : Boolean = ownerNameIsAnon || ownerName.value == "DFiant" || name.value == "$anonfun"
    val value: String = if (isAnonymous) "$anon" else if (nameIsAnon) ownerName.value else name.value
  }
}