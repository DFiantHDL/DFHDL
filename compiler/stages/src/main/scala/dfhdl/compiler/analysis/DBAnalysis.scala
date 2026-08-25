package dfhdl.compiler.analysis
import dfhdl.compiler.ir.*

extension (designDB: DB)
  def getUnusedAnnotValues: List[DFVal] =
    import designDB.getSet
    designDB.members.flatMap:
      case dfVal: DFVal if !dfVal.isAnonymous =>
        // a bit-ranged `Quiet` suppresses only its bits (see `getUnusedBitsAnnotValues`),
        // so it does not put the value on the whole-value suppression list
        val isUnused = dfVal.meta.annotations.exists {
          case u: annotation.Unused => u.bitRangeOpt.isEmpty
          case _                    => false
        }
        if (isUnused) Some(dfVal)
        else None
      case _ => None
  // Named values carrying bit-ranged `Unused.Quiet` annotations, with their ranges in
  // descending order. The compiler mints these in `NamedAliases` for the unread bits of a
  // value it names, and a user may hand-write them; tools turn them into bit-precise waivers.
  def getUnusedBitsAnnotValues: List[(DFVal, List[(Int, Int)])] =
    import designDB.getSet
    designDB.members.flatMap:
      case dfVal: DFVal if !dfVal.isAnonymous =>
        val ranges = dfVal.meta.annotations.flatMap {
          case u: annotation.Unused => u.bitRangeOpt
          case _                    => None
        }
        if (ranges.nonEmpty) Some((dfVal, ranges.sortBy((hi, _) => -hi)))
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
          if !fromVal.isAnonymous && fromVal.getReadDeps.size == 1 =>
        (toVal.widthIntOpt, fromVal.widthIntOpt) match
          case (Some(toWidth), Some(fromWidth)) if toWidth < fromWidth =>
            Some((fromVal, fromWidth - 1, toWidth - 1))
          case _ => None
      case _ => None
  end getUnusedBitsValues
  def getOpenOutPorts: List[DFVal] =
    import designDB.getSet
    // TODO: revisit for interfaces later on
    designDB.members.collect {
      case DFNet.Connection(open: DFVal.Special, from: DFVal, _) if open.isOpen =>
        from
    }
end extension
