package dfhdl.compiler.stages

import dfhdl.compiler.analysis.*
import dfhdl.compiler.ir.*
import dfhdl.compiler.patching.*
import dfhdl.internals.*
import dfhdl.options.CompilerOptions
import scala.collection.mutable
import dfhdl.core.DFOpaque as coreDFOpaque
import dfhdl.core.{asFE, ModifierAny}

/** This stage creates explicit magnet port/var connections across the entire design.
  */
case object ConnectMagnets extends HierarchyStage:
  def dependencies: List[Stage] = List(AddMagnets)
  def nullifies: Set[Stage] = Set(ViaConnection, SimpleOrderMembers)
  // `toPort.asDclAny <> fromPort.asDclAny` uses refTW → getOwnerDesign on
  // Dcls from potentially any design; magnetConnectionMap is also a
  // global (flat) analysis. Run with the outer flat getSet.
  override def rebindGetSet: Boolean = false
  def transformSubDB(subDB: DB)(using MemberGetSet, CompilerOptions, RefGen): DB =
    val design = subDB.top
    val outer = getSet.designDB
    // Collect magnet connections targeting this design (mirrors the
    // classification rules of the flat version, but scoped).
    // For (IN, OUT): connection lives in the design containing both
    // ports' designs — i.e., the parent of `toMP.getOwnerDesign`.
    // DFDesignBlock.ownerRef is Empty under the new model, so walk up
    // via designBlockInstMap (first inst's owner). Singleton hierarchies
    // resolve cleanly; multi-instance picks an arbitrary parent.
    def parentOf(d: DFDesignBlock): Option[DFDesignBlock] =
      outer.designBlockInstMap.get(d).flatMap(_.headOption).map(_.getOwnerDesign)
    val connsForDesign = outer.magnetConnectionMap.iterator.flatMap { case (toMP, fromMP) =>
      val targetDsn =
        if (toMP.isPortIn && (fromMP.isPortIn || fromMP.isVar)) Some(fromMP.getOwnerDesign)
        else if (toMP.isPortOut && fromMP.isPortOut) Some(toMP.getOwnerDesign)
        else if (toMP.isPortIn && fromMP.isPortOut) parentOf(toMP.getOwnerDesign)
        else if (toMP.isPortOut && (fromMP.isPortIn || fromMP.isVar))
          Some(fromMP.getOwnerDesign)
        else None
      if (targetDsn.contains(design)) Some((toMP, fromMP)) else None
    }.toList
    if (connsForDesign.nonEmpty)
      val magnets = connsForDesign.sortBy(_._1.getName)
      val dsn = new MetaDesign(design, Patch.Add.Config.InsideLast):
        extension (mp: ConnectPoint)
          def toDclAny(using MemberGetSet) = mp match
            case ConnectPoint.Via(dfType, dir, designInst, portNamePath) =>
              dfhdl.core.DFVal.PortByNameSelect(dfType, dir, designInst, portNamePath).asDclAny
            case ConnectPoint.Direct(dcl) => dcl.asDclAny
        for ((toMP, fromMP) <- magnets)
          toMP.toDclAny <> fromMP.toDclAny
      subDB.patch(List(dsn.patch))
    else subDB
  end transformSubDB
end ConnectMagnets

extension [T: HasDB](t: T)
  def connectMagnets(using CompilerOptions): DB = StageRunner.run(ConnectMagnets)(t.db)
