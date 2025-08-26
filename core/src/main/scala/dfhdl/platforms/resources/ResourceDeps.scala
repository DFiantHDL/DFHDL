package dfhdl.platforms.resources

trait ResourceDeps extends Resource:
  protected lazy val upstreamDeps: List[Resource]
  override protected[dfhdl] def connectFrom(that: Resource): Unit =
    that match
      case that: ResourceDeps =>
        upstreamDeps.lazyZip(that.upstreamDeps).foreach {
          case (r1, r2) => r1.connectFrom(r2)
        }
      case _ =>
    super.connectFrom(that)
  for (dep <- upstreamDeps)
    dep.addDownstreamDep(this)
