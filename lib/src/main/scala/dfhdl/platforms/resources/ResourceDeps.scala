package dfhdl.platforms.resources

trait ResourceDeps(using RCtx) extends Resource:
  protected def deps: List[Resource]
  override protected[resources] def connect(that: Resource): Unit =
    deps.lazyZip(that.asInstanceOf[ResourceDeps].deps).foreach {
      case (r1, r2) => r1.connect(r2)
    }
    super.connect(that)
