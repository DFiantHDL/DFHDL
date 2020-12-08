package DFiant.lib.stream

sealed trait StreamDir extends Product with Serializable {
  def flip: StreamDir
}
case object SOURCE extends StreamDir {
  def flip: StreamDir = SINK
}
case object SINK extends StreamDir {
  def flip: StreamDir = SOURCE
}
case object FLOW extends StreamDir {
  def flip: StreamDir = FLOW
}
