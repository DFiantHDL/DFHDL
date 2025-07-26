package dfhdl.platforms.resources

trait ResourceDeps(using RCtx) extends Resource:
  protected def deps: List[Resource]
