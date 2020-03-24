package ZFiant.maxeler

import ZFiant._

object Maxeler {
  object Tag {
    case object StreamIOPush extends DFAny.CustomTag {
      override def toString: String = "Maxeler.Tag.StreamIOPush"
    }
    case object StreamIOPull extends DFAny.CustomTag {
      override def toString: String = "Maxeler.Tag.StreamIOPull"
    }
    case object ScalarIO extends DFAny.CustomTag {
      override def toString: String = "Maxeler.Tag.ScalarIO"
    }
  }
}
