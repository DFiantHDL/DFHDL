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
  def transformSubDB(rootDB: DB)(using MemberGetSet, CompilerOptions, RefGen): DB =
    val design = subDB.top
    // owner design + name of each magnet point, precomputed cross-design by the
    // analysis so a ConnectPoint living in another sub-DB is never re-resolved.
    def ownerOf(cp: ConnectPoint): DFDesignBlock = rootDB.magnetPointInfo(cp)._1
    // a design's parent design via the root-aware design tree (no ref resolution).
    // For (IN, OUT) the connection lives in the design containing both ports'
    // designs — i.e. the parent of `toMP`'s design.
    def parentOf(d: DFDesignBlock): Option[DFDesignBlock] =
      rootDB.designBlockOwnershipMap.get(d).flatMap(_.headOption)
    val connsForDesign = rootDB.magnetConnectionMap.iterator.flatMap { case (toMP, fromMP) =>
      val targetDsn =
        if (toMP.isPortIn && (fromMP.isPortIn || fromMP.isVar)) Some(ownerOf(fromMP))
        else if (toMP.isPortOut && fromMP.isPortOut) Some(ownerOf(toMP))
        else if (toMP.isPortIn && fromMP.isPortOut) parentOf(ownerOf(toMP))
        else if (toMP.isPortOut && (fromMP.isPortIn || fromMP.isVar)) Some(ownerOf(fromMP))
        else None
      if (targetDsn.contains(design)) Some((toMP, fromMP)) else None
    }.toList
    if (connsForDesign.nonEmpty)
      val magnets = connsForDesign.sortBy { case (toMP, _) => rootDB.magnetPointInfo(toMP)._2 }
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
