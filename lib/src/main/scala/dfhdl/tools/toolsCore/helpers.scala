package dfhdl.tools.toolsCore

extension (flagVal: Boolean)
  def toFlag(flagName: String): String =
    if (flagVal) s" $flagName"
    else ""
