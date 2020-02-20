package ZFiant
package compiler.sync

protected[sync] trait ClkRstDesign extends MetaDesign {
  private var _hasClk = false
  private var _hasRst = false
  final lazy val clk = {
    _hasClk = true
    DFBool() <> IN addCustomTag(SyncCustomTag.Clock)
  }
  final lazy val rst = {
    _hasRst = true
    DFBool() <> IN addCustomTag(SyncCustomTag.Reset)
  }
  final def hasClk : Boolean = _hasClk
  final def hasRst : Boolean = _hasRst
}
