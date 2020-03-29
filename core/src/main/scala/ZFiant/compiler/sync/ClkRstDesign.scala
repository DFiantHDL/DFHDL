package ZFiant
package compiler.sync

import ZFiant.EdgeDetect.Edge
import ZFiant.compiler.sync.ResetParams.Active

protected[sync] abstract class ClkRstDesign(clkParams : ClockParams, rstParams : ResetParams, simulation : Boolean)(implicit ctx : ContextOf[ClkRstDesign]) extends MetaDesign {
  private var _hasClk = false
  private var _hasRst = false
  private val clkInit : Boolean = clkParams.edge match {
    case Edge.Rising => false
    case Edge.Falling => true
  }
  private val rstInit : Boolean = rstParams.active match {
    case Active.Low => false
    case Active.High => true
  }
  final lazy val clk = {
    _hasClk = true
    if (simulation) DFBit().init(0).setName(clkParams.name) !! Sync.Tag.Clk
    else DFBit() <> IN !! Sync.Tag.Clk setName(clkParams.name)
  }
  final lazy val rst = {
    _hasRst = true
    if (simulation) DFBit().init(rstInit).setName(rstParams.name) !! Sync.Tag.Rst
    else DFBit() <> IN !! Sync.Tag.Rst setName(rstParams.name)
  }
  final def hasClk : Boolean = _hasClk
  final def hasRst : Boolean = _hasRst
}
