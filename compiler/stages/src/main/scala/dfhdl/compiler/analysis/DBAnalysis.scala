package dfhdl.compiler.analysis
import dfhdl.compiler.ir.*

extension (designDB: DB)
  def getUnusedAnnotValues: List[DFVal] =
    import designDB.getSet
    designDB.members.flatMap:
      case dfVal: DFVal if !dfVal.isAnonymous =>
        val isUnused = dfVal.meta.annotations.exists {
          case u: dfhdl.hw.unused => u.isActive
          case _                  => false
        }
        if (isUnused) Some(dfVal)
        else None
      case _ => None
  // TODO: need to apply a more stable tag when converting from mutable to immutable
  def getUnusedParamAnnotValues: List[DFVal] =
    import designDB.getSet
    designDB.members.collect:
      case dfVal: DFVal.DesignParam if dfVal.wasConstDataAccessed => dfVal
  def getUnusedBitsValues: List[(DFVal, Int, Int)] =
    import designDB.getSet
    designDB.members.flatMap:
      case net @ DFNet.Assignment(toVal, DFVal.Alias.AsIs(relValRef = DFRef(fromVal)))
          if !fromVal.isAnonymous && fromVal.getReadDeps.size == 1 &&
            toVal.width < fromVal.width => // && fromVal.tags.hasTagOf[TruncateTag]
        Some(fromVal, fromVal.width - 1, toVal.width)
      case _ => None
  end getUnusedBitsValues
  def getOpenOutPorts: List[DFVal] =
    import designDB.getSet
    // TODO: revisit for interfaces later on
    designDB.members.collect { case DFNet.Connection(_: DFVal.OPEN, from: DFVal, _) =>
      from
    }
end extension
