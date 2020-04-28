package DFiant.lib.stream

sealed trait StreamDir extends Product with Serializable
case object SOURCE extends StreamDir
case object SINK extends StreamDir
case object FLOW extends StreamDir
