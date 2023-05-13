package dfhdl.compiler.ir
import dfhdl.internals.*
opaque type ConnectToMap = Map[DFVal.Dcl, RangeMap[DFNet]]

object ConnectToMap:
  def empty: ConnectToMap = Map()
  extension (ctm: ConnectToMap)(using MemberGetSet)
    def getNets(dcl: DFVal.Dcl, range: Range): Set[DFNet] =
      ctm.getOrElse(dcl, RangeMap.empty).get(range)
    def getNets(dfVal: DFVal): Set[DFNet] =
      dfVal.departialDcl match
        case Some((dcl, range)) => getNets(dcl, range)
        case _                  => Set()
    def addNet(dcl: DFVal.Dcl, range: Range, net: DFNet): ConnectToMap =
      val rangeMap = ctm.getOrElse(dcl, RangeMap.empty)
      ctm + (dcl -> rangeMap.insert(range, net))
    def removeAssignments: ConnectToMap =
      ctm.view
        // keep only connections
        .map((dcl, rangeMap) => (dcl, rangeMap.filter(_.isConnection)))
        // keep non empty range map entries
        .filter(_._2.nonEmpty).toMap
    def contains(dfVal: DFVal): Boolean = getNets(dfVal).nonEmpty
  end extension
end ConnectToMap
