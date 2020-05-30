package DFiant.lib.maxeler

import DFiant._

object Maxeler {
  case object StreamIOPush extends DFAny.CustomTag {
    override def toString: String = "Maxeler.StreamIOPush"
  }
  case object StreamIOPull extends DFAny.CustomTag {
    override def toString: String = "Maxeler.StreamIOPull"
  }
  case object ScalarIO extends DFAny.CustomTag {
    override def toString: String = "Maxeler.ScalarIO"
  }
}
