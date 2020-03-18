package ZFiant
package compiler.sync

protected[sync] trait ClkRstDesign extends MetaDesign {
  private var _hasClk = false
  private var _hasRst = false
  final lazy val clk = {
    _hasClk = true
    DFBit() <> IN !! Sync.Tag.Clk
  }
  final lazy val rst = {
    _hasRst = true
    DFBit() <> IN !! Sync.Tag.Rst
  }
  final def hasClk : Boolean = _hasClk
  final def hasRst : Boolean = _hasRst
}
