package DFiant.compiler.ir

trait NamedGlobal:
  protected val name: String
  def getName(using getSet: MemberGetSet): String = getSet.getGlobalTag[NameTag](this) match
    case Some(NameTag(taggedName)) => taggedName
    case _                         => name
