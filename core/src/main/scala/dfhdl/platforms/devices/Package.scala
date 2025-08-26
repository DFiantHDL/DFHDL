package dfhdl.platforms.devices
import dfhdl.platforms.resources.ResourceOwner

trait Package extends ResourceOwner:
  lazy val packageName: String
