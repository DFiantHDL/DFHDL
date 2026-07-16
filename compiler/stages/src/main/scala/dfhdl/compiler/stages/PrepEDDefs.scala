package dfhdl.compiler.stages

import dfhdl.compiler.analysis.*
import dfhdl.compiler.ir.*
import dfhdl.compiler.patching.*
import dfhdl.internals.*
import dfhdl.options.CompilerOptions

//format: off
/** This stage prepares ED method (HDL function) calls for backend printing.
  *
  * ==Rule 1: Named method calls become variables==
  *
  * A named ED method call (`val x = f(a)`) has no direct HDL equivalent, since HDL function
  * calls are expressions. The call instance is anonymized and a variable carrying the call's
  * name is assigned (inside a process) or connected (at design level) from the call result,
  * so the printed HDL declares the variable and inlines the call exactly once at its
  * assignment, while all readers reference the variable.
  * {{{
  * // Before
  * process(all):
  *   val x = add(a, b)
  *   y := x + x
  *
  * // After
  * process(all):
  *   val x = UInt(8) <> VAR
  *   x := add(a, b)
  *   y := x + x
  * }}}
  */
//format: on
case object PrepEDDefs extends HierarchyStage:
  def dependencies: List[Stage] = List(ExplicitNamedVars)
  def nullifies: Set[Stage] = Set(DropUnreferencedAnons)

  def transformSubDB(rootDB: DB)(using MemberGetSet, CompilerOptions, RefGen): DB =
    val patches = subDB.members.view.flatMap {
      case inst: DFDesignInst if !inst.isAnonymous && inst.getDesignBlock.isHDLSubprogram =>
        // every READ of the named call result has its own out port-by-name select member
        subDB.designInstPBNS.getOrElse(inst, Nil).filter(_.isOut) match
          case mainPbns :: restPbns =>
            val mainReadDeps = mainPbns.getReadDeps.asInstanceOf[Set[DFMember]]
            val dsn = new MetaDesign(
              inst,
              Patch.Add.Config.After,
              // RT domain context is used (as in ExplicitNamedVars) so that `:=` is
              // accepted by the frontend guards; the emitted IR is domain-agnostic
              dfhdl.core.DomainType.RT
            ):
              val plantedNewVar =
                mainPbns.asValAny.dfType.<>(VAR)(using dfc.setMeta(inst.meta))
              if (inst.isInProcess)
                plantedNewVar := mainPbns.asValAny
              else
                plantedNewVar <> mainPbns.asValAny
            List(
              dsn.patch,
              // pre-existing readers of the call result now read the variable instead;
              // the variable assignment/connection created above keeps the single
              // remaining reference to the call
              mainPbns -> Patch.Replace(
                dsn.plantedNewVar.asIR,
                Patch.Replace.Config.ChangeRefOnly,
                Patch.Replace.RefFilter.OfMembers(mainReadDeps)
              ),
              // anonymizing the call instance inlines the call at that reference
              inst -> Patch.Replace(inst.anonymize, Patch.Replace.Config.FullReplacement)
            ) ++
              // the remaining reads' port selects are dropped, redirecting their
              // readers to the variable
              restPbns.map(pbns =>
                pbns -> Patch.Replace(
                  dsn.plantedNewVar.asIR,
                  Patch.Replace.Config.ChangeRefAndRemove
                )
              )
          // a Unit-return (procedural) method call has no result to name — Phase 2
          case Nil => Nil
      case _ => Nil
    }.toList
    subDB.patch(patches)
  end transformSubDB
end PrepEDDefs

extension [T: HasDB](t: T)
  def prepEDDefs(using CompilerOptions): DB =
    StageRunner.run(PrepEDDefs)(t.db)
