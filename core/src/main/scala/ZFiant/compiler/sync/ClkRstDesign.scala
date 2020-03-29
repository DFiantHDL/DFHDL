package ZFiant
package compiler.sync

import ZFiant.EdgeDetect.Edge
import ZFiant.compiler.sync.ResetParams.Active

protected[sync] abstract class ClkRstDesign(clkParams : ClockParams, rstParams : ResetParams, simulation : Boolean)(implicit ctx : ContextOf[ClkRstDesign]) extends MetaDesign {
  private var _hasClk = false
  private var _hasRst = false
  private val clkInit : Int = clkParams.edge match {
    case Edge.Rising => 0
    case Edge.Falling => 1
  }
  private val rstInit : Int = rstParams.active match {
    case Active.Low => 0
    case Active.High => 1
  }
  final lazy val clk = {
    _hasClk = true
    if (simulation) DFBit().setTags(t => t.copy(init = Some(Seq(DFBool.Token(clkInit))))).asInstanceOf[DFBit].setName(clkParams.name) !! Sync.Tag.Clk
    else DFBit() <> IN !! Sync.Tag.Clk setName(clkParams.name)
  }
  final lazy val rst = {
    _hasRst = true
    if (simulation) DFBit().setTags(t => t.copy(init = Some(Seq(DFBool.Token(rstInit))))).asInstanceOf[DFBit].setName(rstParams.name) !! Sync.Tag.Rst
    else DFBit() <> IN !! Sync.Tag.Rst setName(rstParams.name)
  }
  final def hasClk : Boolean = _hasClk
  final def hasRst : Boolean = _hasRst
}
