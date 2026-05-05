package dfhdl.compiler.stages

import dfhdl.compiler.analysis.*
import dfhdl.compiler.ir.*
import dfhdl.compiler.patching.*
import dfhdl.internals.*
import dfhdl.options.CompilerOptions

case object ExplicitState extends HierarchyStage:
  def dependencies: List[Stage] = List(ExplicitNamedVars, DropLocalDcls)
  def nullifies: Set[Stage] = Set()

  def transformSubDB(rootDB: DB)(using
      getSet: MemberGetSet,
      co: CompilerOptions,
      rg: RefGen
  ): DB =
    // Root sub-DB has no design locals (just [globals, topDsn]); the
    // analysis would consume topDsn's outPorts against an empty scopeMap
    // and fail. The actual iteration happens in topDsn's own sub-DB.
    if (subDB.membersNoGlobals.sizeIs <= 1) return subDB
    val patchList = subDB.getImplicitStateVarsDF.view.flatMap {
      // for initialized ports and variables we just add an explicit prev self-assignment
      case e: DFVal.Dcl if e.initRefList.nonEmpty =>
        Some(
          new MetaDesign(e, Patch.Add.Config.After):
            e.asVarAny := e.asValAny.asInitialized.prev
          .patch
        )
      // if not initialized we also need to add bubble tagging to the initialization
      case e: DFVal.Dcl =>
        val explicitStateAssignDsn = new MetaDesign(e, Patch.Add.Config.After):
          val modifier = dfhdl.core.Modifier(e.modifier)
          val dfType = new dfhdl.core.DFType(e.dfType)
          val bubble = dfhdl.core.Bubble.constValOf(dfType, named = false)
          val dclWithInit =
            dfhdl.core.DFVal.Dcl(dfType, modifier, List(bubble))(using
              dfc.setMeta(e.meta)
            ).asVarAny.asInitialized
          dclWithInit := dclWithInit.prev
        val dclPatch = e -> Patch.Replace(
          explicitStateAssignDsn.dclWithInit.asIR,
          Patch.Replace.Config.ChangeRefAndRemove
        )
        List(dclPatch, explicitStateAssignDsn.patch)
      case _ => None
    }.toList
    subDB.patch(patchList)
  end transformSubDB
end ExplicitState

extension [T: HasDB](t: T)
  def explicitState(using CompilerOptions): DB =
    StageRunner.run(ExplicitState)(t.db)
