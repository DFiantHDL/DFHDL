package dfhdl.platforms.resources

trait DiffPair(using RCtx) extends ResourceDeps:
  val pPin: IO
  val nPin: IO
  protected def deps: List[Resource] = List(pPin, nPin)
