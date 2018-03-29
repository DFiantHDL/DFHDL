package DFiant.internals

trait Nameable {
  protected var nameOption : Option[String] = None
  protected def getName : String = nameOption.get
  def setName(name : String) : this.type = {nameOption = Some(name); this}
}
