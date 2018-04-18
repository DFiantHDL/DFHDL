package DFiant.internals

trait Nameable {
  protected[DFiant] var nameOption : Option[String] = None
  def getName : String = nameOption match {
    case Some(n) => n
    case None => "???"
  }
  def setName(name : String) : this.type = {nameOption = Some(name); this}
}


trait NameIt {
  val value : String
}
object NameIt {
  implicit def ev(implicit n : sourcecode.Name) : NameIt = new NameIt {
    val value: String = n.value
  }
}