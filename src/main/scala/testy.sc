abstract class DSN(implicit val parent : Option[DSN] = None) {
  val name : String
  protected implicit val childParent = Some(this)

  override def toString: String = name
}

val haim = new DSN() {
  override val name: String = "Haim"
  val moshe = new DSN() {
    override val name: String = "Moshe"
  }
}

haim.parent
haim.moshe.parent