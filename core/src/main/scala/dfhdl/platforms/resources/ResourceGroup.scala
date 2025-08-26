package dfhdl.platforms.resources

trait ResourceGroup extends Resource, ResourceOwner:
  override protected[dfhdl] def connectFrom(that: Resource): Unit =
    that match
      case that: ResourceGroup =>
        getResources.lazyZip(that.getResources).foreach {
          // skipping ResourceDeps in the group, since their dependencies are already connected
          // since they are also children of the same group.
          // (only when directly connecting two ResourceDeps, their dependencies are connected)
          case (r1: ResourceDeps, r2: ResourceDeps) => // do nothing
          case (r1, r2)                             => r1.connectFrom(r2)
        }
      case _ =>
    super.connectFrom(that)
