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
  val value : String
}
object NameIt {
  implicit def ev(implicit name : sourcecode.Name, ownerName : sourcecode.OwnerName) : NameIt = new NameIt {
    val value: String =
      if (name.value == "$anon") ownerName.value
      else if (name.value == "$anonfun") s"${Name.AnonStart}anon" //loops
      else name.value
//    println(s"${name.value}, ${ownerName.value}, $value")
  }
}

object Name {
  final val AnonStart : String = "ǂ" //"dFt_"
  final val Separator : String = "ǂ" //"__"
}