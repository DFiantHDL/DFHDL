package dfhdl.compiler.stages

import dfhdl.compiler.analysis.*
import dfhdl.compiler.ir.*
import dfhdl.compiler.patching.*
import dfhdl.internals.*
import dfhdl.options.CompilerOptions

case object ExplicitState extends Stage:
  def dependencies: List[Stage] = List(ExplicitNamedVars, DropLocalDcls)
  def nullifies: Set[Stage] = Set()

  def transform(designDB: DB)(using MemberGetSet, CompilerOptions): DB =
    val patchList = designDB.getImplicitStateVarsDF.view.flatMap {
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
    designDB.patch(patchList)
  end transform
end ExplicitState

extension [T: HasDB](t: T)
  def explicitState(using CompilerOptions): DB =
    StageRunner.run(ExplicitState)(t.db)
