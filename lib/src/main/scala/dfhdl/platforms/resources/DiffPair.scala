package dfhdl.platforms.resources

trait DiffPair extends ResourceDeps:
  val pPin: IO
  val nPin: IO
  protected def deps: List[Resource] = List(pPin, nPin)
