package DFiant.lib.maxeler

import DFiant._

object Maxeler {
  case object StreamIOPush extends DFMember.CustomTagOf[DFAny.Member] {
    override def toString: String = "Maxeler.StreamIOPush"
  }
  case object StreamIOPull extends DFMember.CustomTagOf[DFAny.Member] {
    override def toString: String = "Maxeler.StreamIOPull"
  }
  case object ScalarIO extends DFMember.CustomTagOf[DFAny.Member] {
    override def toString: String = "Maxeler.ScalarIO"
  }
}
