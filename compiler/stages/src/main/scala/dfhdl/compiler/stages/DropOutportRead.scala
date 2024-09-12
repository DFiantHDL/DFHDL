package dfhdl.compiler.stages

import dfhdl.compiler.analysis.*
import dfhdl.compiler.ir.*
import dfhdl.compiler.patching.*
import dfhdl.options.CompilerOptions
import dfhdl.compiler.stages.vhdl.VHDLDialect

/** This stage drops reading internally from an output port, by creating an intermediate variable.
  * This is typically required by backends like vhdl.v93 that cannot read from output
  */
case object DropOutportRead extends Stage:
  override def dependencies: List[Stage] = List(ExplicitNamedVars)
  override def nullifies: Set[Stage] = Set()
  override def runCondition(using co: CompilerOptions): Boolean =
    co.backend match
      case be: dfhdl.backends.vhdl =>
        be.dialect match
          case VHDLDialect.v93 => true
          case _               => false
      case _ => false
  def transform(designDB: DB)(using getSet: MemberGetSet, co: CompilerOptions): DB =
    val patchList: List[(DFMember, Patch)] =
      designDB.members.view
        .flatMap {
          // go through all output ports that are read from within their design
          case port @ DclOut() if port.getReadDeps.exists(_.isSameOwnerDesignAs(port)) =>
            val dsn = new MetaDesign(
              port,
              Patch.Add.Config.ReplaceWithFirst(
                Patch.Replace.Config.ChangeRefOnly,
                Patch.Replace.RefFilter.Inside(port.getOwnerDesign)
              )
            ):
              val port_sig = port.asDclAny.genNewVar(using
                dfc.setMeta(port.meta.setName(s"${port.getName}_sig"))
              )
              port_sig.asDclAny <> port.asValAny
            Some(dsn.patch)
          case _ => None
        }
        .toList
    designDB.patch(patchList)
  end transform
end DropOutportRead

extension [T: HasDB](t: T)
  def dropOutportRead(using co: CompilerOptions): DB =
    StageRunner.run(DropOutportRead)(t.db)
