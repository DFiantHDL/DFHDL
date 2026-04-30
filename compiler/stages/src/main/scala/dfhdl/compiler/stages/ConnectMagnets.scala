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
    // Each sub-DB patches ONLY its own design. Skip root, whose designBlock
    // aliases the top sub-DB's designBlock (which also processes it).
    val designOpt = subDB.designBlock.filterNot(d => subDB.internalDBs.contains(d.ownerRef))
    designOpt match
      case Some(design) =>
        val outer = getSet.designDB
        // Collect magnet connections targeting this design (mirrors the
        // classification rules of the flat version, but scoped).
        val connsForDesign = outer.magnetConnectionMap.iterator.flatMap { case (toMP, fromMP) =>
          val targetDsn =
            if (toMP.isPortIn && (fromMP.isPortIn || fromMP.isVar)) Some(fromMP.getOwnerDesign)
            else if (toMP.isPortOut && fromMP.isPortOut) Some(toMP.getOwnerDesign)
            else if (toMP.isPortIn && fromMP.isPortOut) Some(toMP.getOwnerDesign.getOwnerDesign)
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
      case None => subDB
    end match
  end transformSubDB
end ConnectMagnets

extension [T: HasDB](t: T)
  def connectMagnets(using CompilerOptions): DB = StageRunner.run(ConnectMagnets)(t.db)
