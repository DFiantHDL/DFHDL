package DFiant.internals

trait Nameable {
  private[DFiant] def nameDefault : String = "???"
  private var nameManual : String = ""
  private var nameAutoFunc : () => String = () => ""
  private lazy val nameAuto : String = nameAutoFunc()
  private[DFiant] val nameIt : NameIt
  final def hasName : Boolean = !nameManual.isEmpty || !nameAuto.isEmpty
  final lazy val name : String = getUniqueName (
    if (!nameManual.isEmpty) nameManual
    else if (!nameAuto.isEmpty) nameAuto
    else nameDefault
  )
  private[internals] def getUniqueName(suggestedName : String) : String
  final def setName(name : String) : this.type = {nameManual = name; this}
  final protected[DFiant] def setAutoName(name : => String) : this.type = {nameAutoFunc = () => name; this}
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
}
object NameIt {
  private var lastFullName : String = ""
  private var lastNameIt : NameIt = _
  import singleton.ops._
  type ForceNotVar[Sym] = RequireMsgSym[![ImplicitFound[sourcecode.IsVar]], "Do not use `var` for DFiant values", Sym]
  implicit def ev(implicit name : sourcecode.Name, ownerKind : sourcecode.OwnerKind, fullName : sourcecode.FullName)
  : NameIt = new NameIt {
    invalidateName = ownerKind.value match {
      case sourcecode.OwnerKind.Lzy => false
      case sourcecode.OwnerKind.Val => false
      case sourcecode.OwnerKind.Var => false
      case sourcecode.OwnerKind.Obj => false
      case _ => true
    }
    val lastNameIt : NameIt = NameIt.lastNameIt
    val lastFullName : String = NameIt.lastFullName
    lazy val value: String = {
      if (lastFullName == fullName.value)
        lastNameIt.invalidateName = true

      if (invalidateName) s"${Name.AnonStart}anon" else name.value
    }
    lazy val invalidated : Boolean = invalidateName
    NameIt.lastFullName = fullName.value
    NameIt.lastNameIt = this
//    println(s"${name.value}, ${ownerKind.value}, $value")
  }
}

object Name {
  final val AnonStart : String = "dFt_"
  final val Separator : String = "_d_" //"ǂ"
}