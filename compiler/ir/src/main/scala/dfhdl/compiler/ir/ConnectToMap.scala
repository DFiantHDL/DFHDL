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

    /** All nets whose slice overlaps `slice` on `connectToVal`, each with its overlap verdict:
      * `Tri.Yes` for a proven overlap, `Tri.Unknown` when the relation could not be proven either
      * way (conservatively included). Provably disjoint nets are excluded.
      */
    def getNetsVerdicts(connectToVal: ConnectToVal, slice: Slice): Vector[(DFNet, Tri)] =
      ctm.get(connectToVal) match
        case Some(entry) =>
          val widthOpt = connectToVal.widthIntOpt
          entry.nets.view
            .map { (storedSlice, net) =>
              (net, ConnectToMap.overlapsSlices(storedSlice, slice, widthOpt))
            }
            .filter(_._2 != Tri.No)
            .toVector
        case None => Vector.empty

    /** All nets whose slice overlaps `slice` on `dcl`, including ones whose overlap status is
      * merely `Unknown` (conservative).
      */
    def getNets(connectToVal: ConnectToVal, slice: Slice): Set[DFNet] =
      getNetsVerdicts(connectToVal, slice).view.map(_._1).toSet
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
  private def overlapsSlices(a: Slice, b: Slice, widthOpt: Option[Int])(using MemberGetSet): Tri =
    (a, b) match
      case (Slice.Concrete(ra), Slice.Concrete(rb)) =>
        if (ra.intersect(rb).nonEmpty) Tri.Yes else Tri.No
      case (Slice.Concrete(r), Slice.Full) =>
        if (r.isEmpty) Tri.No else Tri.Yes
      case (Slice.Full, Slice.Concrete(r)) =>
        if (r.isEmpty) Tri.No else Tri.Yes
      case (Slice.Full, Slice.Full) => Tri.Yes
      // a symbolic slice is a valid (nonempty) selection, so it always overlaps the full value
      case (_: Slice.Symbolic, Slice.Full) | (Slice.Full, _: Slice.Symbolic) => Tri.Yes
      case (Slice.Symbolic(loA, wA), Slice.Symbolic(loB, wB))                =>
        symbolicOverlap(loA, wA, loB, wB)
      case (Slice.Symbolic(loA, wA), Slice.Concrete(rb)) =>
        import IntExprCalc.DataCalc.const
        symbolicOverlap(loA, wA, const(rb.start), const(rb.length))
      case (Slice.Concrete(ra), Slice.Symbolic(loB, wB)) =>
        import IntExprCalc.DataCalc.const
        symbolicOverlap(const(ra.start), const(ra.length), loB, wB)
      case _ => Tri.Unknown

  /** Overlap of `[loA, loA + wA)` and `[loB, loB + wB)` decided on the linear forms, for every
    * valid parameter assignment. The slice widths serve as the `>= 1` facts for the inequality
    * proofs (see [[IntExprCalc.DataCalc.proveNonNeg]]).
    */
  private def symbolicOverlap(
      loA: IntExprCalc.Linear,
      wA: IntExprCalc.Linear,
      loB: IntExprCalc.Linear,
      wB: IntExprCalc.Linear
  )(using MemberGetSet): Tri =
    import IntExprCalc.DataCalc.*
    val facts = List(wA, wB)
    def nonNeg(e: IntExprCalc.Linear): Boolean = proveNonNeg(e, facts)
    // disjoint when one slice provably ends before the other begins:
    // hiA < loB  <=>  loB - loA - wA >= 0 (and symmetrically)
    if (nonNeg(sub(sub(loB, loA), wA)) || nonNeg(sub(sub(loA, loB), wB))) Tri.No
    // overlapping when each slice provably begins no later than the other ends:
    // loB <= hiA  <=>  loA + wA - 1 - loB >= 0 (and symmetrically)
    else if (
      nonNeg(addConst(sub(add(loA, wA), loB), -1)) &&
      nonNeg(addConst(sub(add(loB, wB), loA), -1))
    ) Tri.Yes
    else Tri.Unknown
  end symbolicOverlap
end ConnectToMap
