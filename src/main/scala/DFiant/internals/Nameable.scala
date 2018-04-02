package DFiant.internals

trait Nameable {
  protected[DFiant] var nameOption : Option[String] = None
  protected[DFiant] def getName : String = nameOption.get
  def setName(name : String) : this.type = {nameOption = Some(name); this}
}
