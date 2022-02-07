package DFiant.internals

trait OnCreateEvents:
  onCreateStart
  def onCreateStart: Unit = {}
  def onCreateStartLate: Unit = {}
  def onCreateEnd: Unit = {}
  final def onCreate: this.type =
    onCreateEnd
    this
