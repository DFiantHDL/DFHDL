package ZFiant
package compiler.sync

protected[sync] abstract class ClkRstDesign(clkName : String, rstName : String)(implicit ctx : ContextOf[ClkRstDesign]) extends MetaDesign {
  private var _hasClk = false
  private var _hasRst = false
  final lazy val clk = {
    _hasClk = true
    DFBit() <> IN !! Sync.Tag.Clk setName(clkName)
  }
  final lazy val rst = {
    _hasRst = true
    DFBit() <> IN !! Sync.Tag.Rst setName(rstName)
  }
  final def hasClk : Boolean = _hasClk
  final def hasRst : Boolean = _hasRst
}
