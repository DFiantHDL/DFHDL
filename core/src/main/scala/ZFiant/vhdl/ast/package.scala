package ZFiant.vhdl

package object ast {
  private[ast] implicit class Delimiter(s : String) {
    import DFiant.internals.StringExtras
    def delim : String = s.delimRowsBy("  ")
  }
}
