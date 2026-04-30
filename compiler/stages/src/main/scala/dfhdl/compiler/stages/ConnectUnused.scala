package dfhdl.compiler.stages

import dfhdl.compiler.analysis.*
import dfhdl.compiler.ir.*
import dfhdl.compiler.patching.*
import dfhdl.internals.*
import dfhdl.options.CompilerOptions
import scala.collection.mutable
import scala.collection.immutable.ListMap

/** This stage connects design instance output ports annotated with @unused to OPEN.
  */
case object ConnectUnused extends HierarchyStage:
  def dependencies: List[Stage] = List()
  def nullifies: Set[Stage] = Set()
  // `unusedPort.asDclAny <> OPEN` uses `refTW` which calls `getOwnerDesign`
  // on the port Dcl; its ownerRef chain only resolves against the flat
  // refTable, so run with the outer getSet.
  override def rebindGetSet: Boolean = false
  def transformSubDB(subDB: DB)(using
      getSet: MemberGetSet,
      co: CompilerOptions,
      rg: RefGen
  ): DB =
    // Each sub-DB handles its own direct-child design instances (patches anchor
    // at `designInst` with Add.Config.After, which modifies the PARENT's scope).
    // Skip top (not a child) and self (processed by parent's sub-DB).
    val patchList: List[(DFMember, Patch)] = subDB.members.view.collect {
      case designInst: DFDesignInst =>
        val design = designInst.getDesignBlock
        val unusedPorts = design.members(MemberView.Folded).view
          .collect {
            case port: DFVal.Dcl if port.isPort => port
          }.filter {
            _.meta.annotations.exists {
              case _: annotation.Unused => true
              case _                    => false
            }
          }.toList
        if (unusedPorts.nonEmpty)
          val dsn = new MetaDesign(designInst, Patch.Add.Config.After):
            for (unusedPort <- unusedPorts) do
              val pbns = dfhdl.core.DFVal.PortByNameSelect(
                unusedPort.dfType,
                unusedPort.modifier.dir,
                designInst,
                unusedPort.getRelativeName(design)
              ).asDclAny
              pbns <> OPEN
          Some(dsn.patch)
        else None
    }.flatten.toList
    subDB.patch(patchList)
  end transformSubDB
end ConnectUnused

extension [T: HasDB](t: T)
  def connectUnused(using CompilerOptions): DB = StageRunner.run(ConnectUnused)(t.db)
