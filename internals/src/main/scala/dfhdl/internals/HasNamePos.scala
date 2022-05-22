package dfhdl.internals

trait HasNamePos:
  protected def setClsNamePos(name: String, position: Position): Unit

trait HasNamePosWithVars extends HasNamePos:
  private var _clsName: String = ""
  private var _clsPosition: Position = Position.unknown
  final protected def setClsNamePos(name: String, position: Position): Unit =
    _clsName = name
    _clsPosition = position
  final def clsName: String = _clsName
  final def clsPosition: Position = _clsPosition
