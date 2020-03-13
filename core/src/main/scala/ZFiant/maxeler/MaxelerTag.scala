package ZFiant.maxeler

import ZFiant._


sealed trait MaxelerTag extends DFMember.CustomTag
object MaxelerTag {
  case object StreamIOPush extends MaxelerTag {
    override def toString: String = "MaxelerTag.StreamIOPush"
  }
  case object StreamIOPull extends MaxelerTag {
    override def toString: String = "MaxelerTag.StreamIOPull"
  }
  case object ScalarIO extends MaxelerTag {
    override def toString: String = "MaxelerTag.ScalarIO"
  }
}
