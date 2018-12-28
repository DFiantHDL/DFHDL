package DFiant.internals

trait Nameable {
  private[DFiant] def nameDefault : String = "???"
  private var nameManual : String = ""
  private var nameAuto : String = ""
  private[DFiant] val nameIt : NameIt
  final def hasName : Boolean = !nameManual.isEmpty || !nameAuto.isEmpty
  final lazy val name : String = getUniqueName (
    if (!nameManual.isEmpty) nameManual
    else if (!nameAuto.isEmpty) nameAuto
    else nameDefault
  )
  private[internals] def getUniqueName(suggestedName : String) : String
  final def setName(name : String) : this.type = {nameManual = name; this}
  final protected[DFiant] def setAutoName(name : String) : this.type = {nameAuto = name; this}
  override def toString : String = name
}

trait TypeNameable {
  private var typeNameAuto : String = "???"
  lazy val typeName : String = typeNameAuto
  final protected[DFiant] def setAutoTypeName(name : String) : this.type = {typeNameAuto = name; this}
}


trait NameIt {
  protected var invalidateName = false
  val value : String
  val invalidated : Boolean
  val owner : String
}
object NameIt {
  private var lastFullName : String = ""
  private var lastNameIt : NameIt = _
  implicit def ev(implicit name : sourcecode.Name, ownerName : sourcecode.OwnerName, fullName : sourcecode.FullName) : NameIt = new NameIt {
    lazy val value: String = if (name.value.contains("$") || invalidateName) s"${Name.AnonStart}anon" else name.value
    lazy val invalidated : Boolean = invalidateName
    val owner: String = if (ownerName.value.contains("$")) s"${Name.AnonStart}anon" else ownerName.value
    if (lastFullName == fullName.value)
      lastNameIt.invalidateName = true
    lastFullName = fullName.value
    lastNameIt = this
//    println(s"${name.value}, ${ownerName.value}, $value")
  }
}

object Name {
  final val AnonStart : String = "dFt_"
  final val Separator : String = "_d_" //"ǂ"
}