package dfhdl.platforms.resources

trait ResourceGroup(using RCtx) extends Resource, ResourceOwner:
  override protected[resources] def connect(that: Resource): Unit =
    getResources.lazyZip(that.asInstanceOf[ResourceGroup].getResources).foreach {
      // skipping ResourceDeps in the group, since their dependencies are already connected
      // since they are also children of the same group.
      // (only when directly connecting two ResourceDeps, their dependencies are connected)
      case (r1: ResourceDeps, r2: ResourceDeps) => // do nothing
      case (r1, r2)                             => r1.connect(r2)
    }
    super.connect(that)
