package dfhdl.tools.toolsCore

extension (flagVal: Boolean)
  def toFlag(flagName: String): String =
    if (flagVal) flagName
    else ""

extension (param: dfhdl.compiler.ir.DFVal.DesignParam)
  /** The applied data of a vendor IP block's parameter, for emitting the IP generation script.
    *
    * An IP block is a sub-design, so its parameter values live at its instantiation site and only
    * `getConstDataThroughParams` resolves them; the default (`Always`) cache policy deliberately
    * keeps a sub-design parameter opaque (`UnknownConst`). The data itself is `Option`-shaped
    * (`None` is the bubble value), which an IP script cannot express either, so both cases raise an
    * error naming the IP and the parameter.
    */
  def ipParamData(ipName: String)(using dfhdl.compiler.ir.MemberGetSet): Any =
    param.getConstDataThroughParams[Option[Any]].flatten.getOrElse(
      throw new IllegalArgumentException(
        s"The IP `$ipName` parameter `${param.getName}` has no constant value that can be resolved for the IP generation script."
      )
    )
end extension
