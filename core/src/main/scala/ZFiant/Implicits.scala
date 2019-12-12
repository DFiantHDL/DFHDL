package ZFiant

import DFAny._

trait Implicits extends
  DFBits.Op.Implicits with
//  DFUInt.Op.Implicits with
//  DFSInt.Op.Implicits with
//  DFEnum.Op.Implicits with
  DFBool.Op.Implicits {
  implicit class ConnectableOps1[Type <: DFAny.Type](left : ConnectableOf[Type]) {
    def <>[R](right: left.dfType.OpAble[R])(
      implicit ctx: DFNet.Context, op: left.dfType.`Op<>Builder`[Type, R]
    ): Unit = left.connectWith(op(left.dfType, right))
  }
//  implicit class ConnectableOps2[Type <: DFAny.Type](left : ConnectableOf[Type]) {
//    def <>[R](right: left.dfType.OpAble[R])(
//      implicit ctx: DFNet.Context, op: left.dfType.`Op<>Builder`[Type, R]
//    ): Unit = left.connectWith(op(left.dfType, right))
//  }
}
