package DFiant
package compiler.rtl

import constraints.timing.sync._

protected[rtl] abstract class ClkRstDesign(clkParams : ClockParams, rstParams : ResetParams, simulation : Boolean)(
  implicit ctx : ContextOf[ClkRstDesign]
) extends MetaDesign {
  private var _hasClk = false
  private var _hasRst = false
  private val clkInit : Int = clkParams.inactiveInt
  private val rstInit : Int = rstParams.activeInt
  final lazy val clk = {
    _hasClk = true
    if (simulation) DFBit().forcedInit(Seq(DFBool.Token(clkInit))).setName(clkParams.name) !! RTL.Tag.Clk
    else DFBit() <> IN !! RTL.Tag.Clk setName(clkParams.name)
  }
  final lazy val rst = {
    _hasRst = true
    if (simulation) DFBit().forcedInit(Seq(DFBool.Token(rstInit))).setName(rstParams.name) !! RTL.Tag.Rst
    else DFBit() <> IN !! RTL.Tag.Rst setName(rstParams.name)
  }
  final def hasClk : Boolean = _hasClk
  final def hasRst : Boolean = _hasRst
}
