package dfhdl.platforms.resources

final case class Sig(level: IOLevel = IOLevel.LVCMOS33)(using RCtx) extends HasIOLevel
