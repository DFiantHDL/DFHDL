package dfhdl.compiler.ir
import dfhdl.internals.*
opaque type ConnectToMap = Map[DFVal.Dcl, Vector[Option[DFNet]]]

object ConnectToMap:
  def empty: ConnectToMap = Map()
  extension (ctm: ConnectToMap)(using MemberGetSet)
    def dcls: Set[DFVal.Dcl] = ctm.keySet
    def getNets(dcl: DFVal.Dcl, range: Range): Set[DFNet] =
      val vector = ctm.getOrElse(dcl, Vector.fill(dcl.width)(None))
      range.flatMap(vector(_)).toSet
    def getNets(dfVal: DFVal): Set[DFNet] =
      dfVal.departialDcl match
        case Some((dcl, range)) => getNets(dcl, range)
        case _                  => Set()
    def addNet(dcl: DFVal.Dcl, range: Range, net: DFNet): ConnectToMap =
      val vector = ctm.getOrElse(dcl, Vector.fill(dcl.width)(None))
      val newVector = vector.zipWithIndex.map {
        case (_, idx) if range.contains(idx) => Some(net)
        case (elem, _)                       => elem
      }
      ctm + (dcl -> newVector)
    def removeAssignments: ConnectToMap =
      ctm.view
        // keep only connections
        .map { (dcl, vector) =>
          (
            dcl,
            vector.map {
              case Some(net) if net.isConnection => Some(net)
              case _                             => None
            }
          )
        }.toMap
    def contains(dcl: DFVal.Dcl, range: Range): Boolean = getNets(dcl, range).nonEmpty
    def contains(dfVal: DFVal): Boolean = getNets(dfVal).nonEmpty
  end extension
end ConnectToMap
