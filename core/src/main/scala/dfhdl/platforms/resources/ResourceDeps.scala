package dfhdl.platforms.resources

trait ResourceDeps extends Resource:
  protected lazy val upstreamDeps: List[Resource]
  override protected[resources] def connect(that: Resource): Unit =
    upstreamDeps.lazyZip(that.asInstanceOf[ResourceDeps].upstreamDeps).foreach {
      case (r1, r2) => r1.connect(r2)
    }
    super.connect(that)
  for (dep <- upstreamDeps)
    dep.addDownstreamDep(this)
