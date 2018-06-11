package DFiant.internals

trait Nameable {
  private var nameManualOption : Option[String] = None
  private var nameAutoOption   : Option[String] = None
  def hasName : Boolean = nameManualOption match {
    case Some(n) => true
    case None => nameAutoOption match {
      case Some(n) => true
      case None => false
    }
  }
  lazy val name : String = nameManualOption match {
    case Some(n) => n
    case None => nameAutoOption match {
      case Some(n) => n
      case None => "???"
    }
  }
  def setName(name : String) : this.type = {nameManualOption = Some(name); this}
  final protected[DFiant] def setAutoName(name : String) : this.type = {nameAutoOption = Some(name); this}
  override def toString : String = name
}

trait TypeNameable {
  private var typeNameAutoOption : Option[String] = None
  def getTypeName : String = typeNameAutoOption match {
    case Some(n) => n
    case None => "???"
  }
  final protected[DFiant] def setAutoTypeName(name : String) : this.type = {typeNameAutoOption = Some(name); this}
}


trait NameIt {
  val value : String
}
object NameIt {
  implicit def ev(implicit n : sourcecode.Name) : NameIt = new NameIt {
    val value: String = n.value
  }
}