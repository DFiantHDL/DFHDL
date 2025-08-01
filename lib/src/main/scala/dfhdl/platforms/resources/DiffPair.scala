package dfhdl.platforms.resources

trait DiffPair extends ResourceDeps:
  val pPin: IO
  val nPin: IO
  lazy val upstreamDeps: List[Resource] = List(pPin, nPin)
