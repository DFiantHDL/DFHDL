package ZFiant.maxeler

import ZFiant._

sealed trait MaxelerCustomTag extends DFMember.CustomTag
case object MaxelerStreamIOPush extends MaxelerCustomTag
case object MaxelerStreamIOPull extends MaxelerCustomTag
case object MaxelerScalarIO extends MaxelerCustomTag