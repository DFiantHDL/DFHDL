package DFiant.internals

trait Nameable {
  protected[DFiant] var nameManualOption : Option[String] = None
  protected[DFiant] var nameAutoOption   : Option[String] = None
  def hasName : Boolean = nameManualOption match {
    case Some(n) => true
    case None => nameAutoOption match {
      case Some(n) => true
      case None => false
    }
  }
  def getName : String = nameManualOption match {
    case Some(n) => n
    case None => nameAutoOption match {
      case Some(n) => n
      case None => "???"
    }
  }
  def setName(name : String) : this.type = {nameManualOption = Some(name); this}
  final protected[DFiant] def setAutoName(name : String) : this.type = {nameAutoOption = Some(name); this}
  override def toString : String = getName
}


trait NameIt {
  val value : String
}
object NameIt {
  implicit def ev(implicit n : sourcecode.Name) : NameIt = new NameIt {
    val value: String = n.value
  }
}