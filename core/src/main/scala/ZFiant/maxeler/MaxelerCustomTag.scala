package ZFiant.maxeler

import ZFiant._

protected[maxeler] sealed trait MaxelerCustomTag extends DFMember.CustomTag
protected[maxeler] case object MaxelerStreamIOPush extends MaxelerCustomTag
protected[maxeler] case object MaxelerStreamIOPull extends MaxelerCustomTag
protected[maxeler] case object MaxelerScalarIO extends MaxelerCustomTag