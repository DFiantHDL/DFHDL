package dfhdl.internals

trait OnCreateEvents:
  type This
  onCreateStart
  def onCreateStart: Unit = {}
  def onCreateStartLate: Unit = {}
  def onCreateEnd(thisOwner: Option[This]): Unit = {}
  final def onCreate(thisOwner: Option[Any]): this.type =
    onCreateEnd(thisOwner.asInstanceOf[Option[This]])
    this
