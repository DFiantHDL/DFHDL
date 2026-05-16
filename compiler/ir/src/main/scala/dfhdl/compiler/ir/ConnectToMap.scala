package dfhdl.compiler.ir
import dfhdl.internals.*

/** Per-declaration record of all nets (connections and assignments) that touch some slice of the
  * declaration, together with the accumulated coverage that those nets provide.
  */
final case class ConnectToEntry(coverage: Coverage, nets: Vector[(Slice, DFNet)])
object ConnectToEntry:
  val empty: ConnectToEntry = ConnectToEntry(Coverage.empty, Vector.empty)

type ConnectToVal = DFVal.PortByNameSelect | DFVal.Dcl | DFVal.Special
opaque type ConnectToMap = Map[ConnectToVal, ConnectToEntry]

object ConnectToMap:
  def empty: ConnectToMap = Map()
  extension (ctm: ConnectToMap)(using MemberGetSet)
    def connectToVals: Set[ConnectToVal] = ctm.keySet

    /** All nets whose slice overlaps `slice` on `dcl`, including ones whose overlap status is
      * merely `Unknown` (conservative).
      */
    def getNets(connectToVal: ConnectToVal, slice: Slice): Set[DFNet] =
      ctm.get(connectToVal) match
        case Some(entry) =>
          val widthOpt = connectToVal.widthIntOpt
          entry.nets.collect {
            case (storedSlice, net)
                if ConnectToMap.overlapsSlices(storedSlice, slice, widthOpt) != Tri.No =>
              net
          }.toSet
        case None => Set.empty
    def getNets(dfVal: DFVal): Set[DFNet] =
      dfVal.departialPBNS match
        case Some(connectToVal, slice) => getNets(connectToVal, slice)
        case _                         => Set.empty
    def addNet(connectToVal: ConnectToVal, slice: Slice, net: DFNet): ConnectToMap =
      val entry = ctm.getOrElse(connectToVal, ConnectToEntry.empty)
      val newEntry = entry.copy(
        coverage = entry.coverage.assign(slice, connectToVal.widthIntOpt),
        nets = entry.nets :+ ((slice, net))
      )
      ctm + (connectToVal -> newEntry)
    def removeAssignments: ConnectToMap =
      ctm.view
        .map { (connectToVal, entry) =>
          val connectionNets = entry.nets.filter(_._2.isConnection)
          val rebuiltCoverage = connectionNets.foldLeft(Coverage.empty) {
            case (cov, (slice, _)) => cov.assign(slice, connectToVal.widthIntOpt)
          }
          connectToVal -> ConnectToEntry(rebuiltCoverage, connectionNets)
        }.toMap
    def contains(connectToVal: ConnectToVal, slice: Slice): Boolean =
      getNets(connectToVal, slice).nonEmpty
    def contains(dfVal: DFVal): Boolean = getNets(dfVal).nonEmpty

    /** Coverage accumulated on a declaration. [[Coverage.empty]] when the declaration has no entry.
      */
    def coverageOf(connectToVal: ConnectToVal): Coverage =
      ctm.get(connectToVal).map(_.coverage).getOrElse(Coverage.empty)
  end extension

  /** Pairwise slice-overlap predicate used by `getNets`. Returns `Tri.Yes` only when provably
    * overlapping, `Tri.No` only when provably disjoint, `Tri.Unknown` otherwise.
    */
  private def overlapsSlices(a: Slice, b: Slice, widthOpt: Option[Int]): Tri =
    (a, b) match
      case (Slice.Concrete(ra), Slice.Concrete(rb)) =>
        if (ra.intersect(rb).nonEmpty) Tri.Yes else Tri.No
      case (Slice.Concrete(r), Slice.Full) =>
        if (r.isEmpty) Tri.No else Tri.Yes
      case (Slice.Full, Slice.Concrete(r)) =>
        if (r.isEmpty) Tri.No else Tri.Yes
      case (Slice.Full, Slice.Full) => Tri.Yes
      case _                        => Tri.Unknown
end ConnectToMap
