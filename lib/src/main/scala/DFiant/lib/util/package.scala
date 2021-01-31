package DFiant.lib
import DFiant._
import DFDesign.Frontend._

package object util {
  //////////////////////////////////////////////////////////////////////////////////////////////////////////
  //                                           Activation Bit
  //////////////////////////////////////////////////////////////////////////////////////////////////////////
  import ActivationBit._
  final implicit class __DFBitExpand(bit : DFBit.type) {
    val ActiveHigh = ActivationBit.ActiveHigh
    val ActiveLow = ActivationBit.ActiveLow
    val Status = ActivationBit.Status
  }

  final implicit class __ActiveHighOps(bit : ActiveHigh.TVal) {
    @df def isActive : DFBool = bit.actual
    @df def unary_! : ActiveLow.TVal = (!bit.actual).as(ActiveLow)
  }
  final implicit class __ActiveLowOps(bit : ActiveLow.TVal) {
    @df def isActive : DFBool = !bit.actual
    @df def unary_! : ActiveHigh.TVal = (!bit.actual).as(ActiveHigh)
  }

  @df final implicit def __DFBitActiveHigh_ac_DFBitActiveLow : DFAny.`Op:=,<>`.Builder[ActiveHigh.TType, ActiveLow.TVal] = (left, right) => {
    !right
  }
  @df final implicit def __DFBitActiveLow_ac_DFBitActiveHigh : DFAny.`Op:=,<>`.Builder[ActiveLow.TType, ActiveHigh.TVal] = (left, right) => {
    !right
  }
  @df final implicit def __DFBitActiveHigh_TokenStatus[V <: Status]
  : DFAny.Token.ToFit.Summon.SAM[ActiveHigh.TType, V, ActiveHigh.TToken] = (_, value) => value match {
    case Status.Active => ActiveHigh(true)
    case Status.Inactive => ActiveHigh(false)
  }
  @df final implicit def __DFBitActiveLow_TokenStatus[V <: Status]
  : DFAny.Token.ToFit.Summon.SAM[ActiveLow.TType, V, ActiveLow.TToken] = (_, value) => value match {
    case Status.Active => ActiveLow(false)
    case Status.Inactive => ActiveLow(true)
  }
  //////////////////////////////////////////////////////////////////////////////////////////////////////////
}
