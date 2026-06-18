package dfhdl.compiler.stages

import dfhdl.compiler.analysis.*
import dfhdl.compiler.ir.*
import dfhdl.compiler.patching.*
import dfhdl.options.CompilerOptions

/** This stage converts local parameters that are used in IOs to be design parameters with default
  * values, since VHDL does not support local parameters for IO access. These kind of design
  * parameters remain at their default (relative) values and are never directly applied.
  */
case object LocalToDesignParams extends HierarchyStage:
  override def runCondition(using co: CompilerOptions): Boolean =
    co.backend match
      case be: dfhdl.backends.vhdl => true
      case _                       => false
  override def dependencies: List[Stage] = List(
    GlobalizePortVectorParams,
    SimpleOrderMembers
  )
  override def nullifies: Set[Stage] = Set()
  // Per-design: each design converts its OWN IO-used local params to design
  // parameters. The applied value of a non-top design's new param is read from
  // its PARENT design's matching local const (`<instanceName>_<paramName>`,
  // produced upstream) — a cross-design READ via `parentSubDBOpt`, never a
  // cross-design patch. Global params are excluded by `getIOLocalParams`, so
  // there is no shared-global value to coordinate across sub-DBs.
  def transformSubDB(rootDB: DB)(using getSet: MemberGetSet, co: CompilerOptions, rg: RefGen): DB =
    val design = subDB.top
    val isTopTop = design eq rootDB.top
    val ioLocalParams = design.getIOLocalParams
    if (ioLocalParams.isEmpty) subDB
    else
      // local consts of the parent design, used to source the applied value of a
      // non-top design's converted param (see the class comment).
      val parentLocalConsts: Map[String, DFVal.CanBeExpr] =
        if (isTopTop) Map.empty
        else
          subDB.parentSubDBOpt match
            case Some(parentSub) =>
              parentSub.atGetSet {
                parentSub.members.view.collect { case const @ DclConst() =>
                  const.getName -> const
                }.toMap
              }
            case None => Map.empty
      val instName = design.getName
      // adding design parameters with initial values set as the anonymized local parameters
      val patchList: List[(DFMember, Patch)] =
        ioLocalParams.view.collect { lp =>
          val dsn = new MetaDesign(
            lp,
            Patch.Add.Config.ReplaceWithLast(Patch.Replace.Config.FullReplacement)
          ):
            val lpAnon = plantMember(lp.anonymize).asValAny
            val paramValue =
              if (isTopTop) lpAnon
              else
                val paramName = s"${instName}_${lp.getName}"
                parentLocalConsts.get(paramName).getOrElse(lpAnon.asIR).asValAny
            val dp = dfhdl.core.DFVal.DesignParam(paramValue, Some(lpAnon))(using
              dfc.setMeta(lp.meta)
            )
          dsn.patch
        }.toList
      subDB.patch(patchList)
    end if
  end transformSubDB
end LocalToDesignParams

extension [T: HasDB](t: T)
  def localToDesignParams(using co: CompilerOptions): DB =
    StageRunner.run(LocalToDesignParams)(t.db)
