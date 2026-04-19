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
  // Dcls from potentially any design; magnetConnectionTable is also a
  // global (flat) analysis. Run with the outer flat getSet.
  override def rebindGetSet: Boolean = false
  def transformSubDB(subDB: DB)(using
      getSet: MemberGetSet, co: CompilerOptions, rg: RefGen
  ): DB =
    // Each sub-DB patches ONLY its own design. Skip root, whose designBlock
    // aliases the top sub-DB's designBlock (which also processes it).
    val designOpt = subDB.designBlock.filterNot(d => subDB.internalDBs.contains(d.ownerRef))
    designOpt match
      case Some(design) =>
        val outer = getSet.designDB
        // Collect magnet connections targeting this design (mirrors the
        // classification rules of the flat version, but scoped).
        val connsForDesign = outer.magnetConnectionTable.iterator.flatMap { case (toPort, fromDcl) =>
          val targetDsn = (toPort, fromDcl) match
            case (DclIn(), DclIn() | DclVar())  => Some(fromDcl.getOwnerDesign)
            case (DclOut(), DclOut())           => Some(toPort.getOwnerDesign)
            case (DclIn(), DclOut())            => Some(toPort.getOwnerDesign.getOwnerDesign)
            case (DclOut(), DclIn() | DclVar()) => Some(fromDcl.getOwnerDesign)
            case _                              => None
          if (targetDsn.contains(design)) Some((toPort, fromDcl)) else None
        }.toList
        if (connsForDesign.nonEmpty)
          val magnets = connsForDesign.sortBy(_._1.getName)
          val dsn = new MetaDesign(design, Patch.Add.Config.InsideLast):
            for ((toPort, fromPort) <- magnets)
              toPort.asDclAny <> fromPort.asDclAny
          subDB.patch(List(dsn.patch))
        else subDB
      case None => subDB
  end transformSubDB
end ConnectMagnets

extension [T: HasDB](t: T)
  def connectMagnets(using CompilerOptions): DB = StageRunner.run(ConnectMagnets)(t.db)
