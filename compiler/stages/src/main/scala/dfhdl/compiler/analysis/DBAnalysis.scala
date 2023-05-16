package dfhdl.compiler.analysis
import dfhdl.compiler.ir.*

extension (designDB: DB)
  def getUnusedTaggedValues: List[DFVal] =
    import designDB.getSet
    designDB.members.flatMap:
      case dfVal: DFVal if !dfVal.isAnonymous =>
        if (dfVal.meta.annotations.exists { case u: dfhdl.core.hw.unused => u.isActive })
          Some(dfVal)
        else None
      case _ => None
  def getOpenOutPorts: List[DFVal] =
    import designDB.getSet
    designDB.members.flatMap:
      case outPort @ DclOut() if outPort.tags.hasTagOf[OpenConnectTag] => Some(outPort)
      case _                                                           => None
end extension
