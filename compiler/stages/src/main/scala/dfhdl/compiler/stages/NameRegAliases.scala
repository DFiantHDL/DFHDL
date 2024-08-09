package dfhdl.compiler.stages

import dfhdl.compiler.analysis.*
import dfhdl.compiler.ir.DFRef.TwoWay
import dfhdl.compiler.ir.{*, given}
import dfhdl.compiler.patching.*
import dfhdl.options.CompilerOptions
import dfhdl.internals.*
import DFVal.Alias.History.Op as HistoryOp
import scala.annotation.tailrec
import scala.collection.mutable
import dfhdl.core.DomainType.RT
import DFVal.Modifier as IRModifier
import dfhdl.core.RTDomainCfg.Derived

/** This stage names register aliases (e.g., `x.reg`) and replaces them with explicit register
  * variables. The most complex mechanism about this stage is the naming conversion convention.
  *   1. If `.reg` is applied on a named immutable value `x` or a mutated wire/port that is mutated
  *      only once, then that register variable will be named `x_reg`. If we have several register
  *      stages applied, then we create an enumeration. So `x.reg(2)` yields `x_reg1` and `x_reg2`.
  *   1. If `.reg` is applied on a named mutable wire `x` that is mutated more than once, then we
  *      treat every new `.reg` application as a new version of x. In this case we get an
  *      enumeration of the version. E.g.:
  *      {{{
  *        val i = DFUInt(8) <> IN
  *        val o = DFUInt(8) <> OUT
  *        val x = DFUInt(8) <> VAR
  *        x := i
  *        o := x.reg //x_ver1_reg
  *        x := i + 1
  *        o := x.reg(2) //x_ver2_reg1, x_ver2_reg2
  *      }}}
  *   1. If `.reg` is applied on an anonymous value, then extrapolate a name suggestion based on the
  *      destination variable. This is part of the destination, so it adds `_part` suffix to the
  *      name of the destination. In case of several parts, we create an enumeration. E.g.:
  *      {{{
  *        val i = DFUInt(8) <> IN
  *        val o = DFUInt(8) <> OUT
  *        val z = DFUInt(8) <> OUT
  *        o := (i + 1).reg //o_part_reg
  *        z := ((i + 1).reg + 7).reg(2) //z_part1_reg, z_part2_reg1, z_part2_reg2
  *      }}}
  */
case object NameRegAliases extends Stage:
  // We order the members to have declarations first and make sure there are unique names
  // so that the naming system will be more coherent. However, this stage also may cause naming
  // collisions in rare cases, so we also need to nullify the unique name stage.
  def dependencies: List[Stage] = List(DFHDLUniqueNames, SimpleOrderMembers, ExplicitRegInits)
  def nullifies: Set[Stage] = Set(DFHDLUniqueNames)
  final case class NameGroup(name: String, unique: Boolean)
  extension (regAlias: DFVal.Alias.History)(using MemberGetSet)
    @tailrec private def getNonRegAliasRelVal: DFVal =
      regAlias.relValRef.get match
        case anotherAlias: DFVal.Alias.History => anotherAlias.getNonRegAliasRelVal
        case dfVal                             => dfVal
    @tailrec private def getTotalSteps(accumulatedSteps: Int): Int =
      regAlias.relValRef.get match
        case anotherAlias: DFVal.Alias.History =>
          anotherAlias.getTotalSteps(accumulatedSteps + regAlias.step)
        case dfVal => accumulatedSteps + regAlias.step
    private def getTotalSteps: Int = regAlias.getTotalSteps(0)
    private def getNameGroup: NameGroup =
      regAlias.getNonRegAliasRelVal match
        // has reg alias versioning if assigned more than once and is not a REG declaration,
        // since REG dcl assignments are "non-blocking".
        case dcl: DFVal.Dcl if dcl.getAssignmentsTo.size > 1 && !dcl.modifier.isReg =>
          NameGroup(s"${dcl.getName}_ver", true)
        case dfVal: DFVal if dfVal.isAnonymous =>
          dfVal.suggestName.map(NameGroup(_, true)).getOrElse(NameGroup(dfVal.getName, false))
        case dfVal: DFVal => NameGroup(dfVal.getName, false)
  end extension

  def transform(designDB: DB)(using MemberGetSet, CompilerOptions): DB =
    val patchList: List[(DFMember, Patch)] = designDB.namedOwnerMemberList.flatMap {
      case (domainOwner: (DFDomainOwner & DFBlock), members) =>
        val regPatches = mutable.ListBuffer.empty[(DFMember, Patch)]
        def patchRemoveHistoryInit(alias: DFVal.Alias.History): Unit =
          alias.initRefOption.foreach(_.get.collectRelMembers(false).foreach {
            regPatches += _ -> Patch.Remove()
          })
        // A reg alias that is already properly named should be ignored.
        // A reg alias is properly named when there is single assignment to a
        // DFHDL variable from a single-step reg alias.
        val properAliasesPatches = mutable.ListBuffer.empty[(DFMember, Patch)]
        val properAliases = members.view.collect {
          case DFNet.Assignment(
                dcl: DFVal.Dcl,
                regAlias @ DFVal.Alias.History(_, DFRef(relVal), 1, _, _, _, _, _)
              ) if regAlias.isAnonymous && dcl.getAssignmentsTo.size == 1 =>
            patchRemoveHistoryInit(regAlias)
            val dsn = new MetaDesign(dcl, Patch.Add.Config.ReplaceWithLast(), RT(Derived)):
              val modifierFE = dcl.modifier.dir match
                case IRModifier.VAR   => VAR.REG
                case IRModifier.OUT   => OUT.REG
                case IRModifier.INOUT => INOUT.REG
                case _                => ??? // impossible
              val clonedInitList =
                regAlias.initOption.map(_.cloneAnonValueAndDepsHere.asConstAny).toList
              val regDcl = dcl.asValAny.dfType.<>(modifierFE).initForced(clonedInitList)(using
                dfc.setMeta(dcl.meta)
              )
            properAliasesPatches += dsn.patch
            properAliasesPatches += regAlias -> Patch.Replace(
              relVal,
              Patch.Replace.Config.ChangeRefAndRemove
            )
            regAlias
        }.toSet
        val nameGroupRegMap =
          members.view
            .collect {
              case regAlias @ DFVal.Alias.History(_, _, _, HistoryOp.State, _, _, _, _)
                  if !properAliases.contains(regAlias) =>
                regAlias
            }
            .groupByOrdered(_.getNameGroup)

        // assumes we ordered the members so the declarations come first.
        // if there are no declarations, we set the owner as position.
        // we keep in mind that the declaration may have anonymous value
        // representing the initialized values among them, before the declarations,
        // so we go from the bottom to the top searching for the first declaration.
        val (posMember, addCfg): (DFMember, Patch.Add.Config) = members.view.reverse.dropWhile {
          case dcl: DFVal.Dcl if dcl.getOwnerDomain == domainOwner => false
          case _                                                   => true
        }.headOption match
          case Some(lastDcl) => (lastDcl, Patch.Add.Config.After)
          case None          => (domainOwner, Patch.Add.Config.InsideFirst)
        val regDsn = new MetaDesign(posMember, addCfg, domainType = RT(Derived)):
          def addRegs(
              aliases: List[DFVal.Alias.History],
              namePrefix: String,
              maxRegs: Int,
              unique: Boolean
          ): List[DFVal] =
            val alias = aliases.head
            val regs = for (i <- 1 to maxRegs) yield
              val nameSuffix =
                if (maxRegs == 1) "_reg"
                else s"_reg${i.toPaddedString(maxRegs)}"
              val regName =
                if (i == maxRegs && !alias.isAnonymous) alias.getName
                else namePrefix + nameSuffix
              val clonedInitList =
                alias.initOption.map(_.cloneAnonValueAndDepsHere.asConstAny).toList
              alias.asValAny.dfType.<>(VAR.REG).initForced(clonedInitList)(using
                dfc.setMeta(alias.meta.setName(regName))
              )
            val regsIR = regs.map(_.asIR).toList
            val relVal = alias.getNonRegAliasRelVal
            var initOptions = aliases.flatMap(a => List.fill(a.step)(a.initOption))
            def regDinPatch(posMember: DFMember, addCfg: Patch.Add.Config) =
              new MetaDesign(posMember, addCfg, domainType = RT(Derived)):
                (relVal :: regsIR).lazyZip(regsIR).foreach { (prev, curr) =>
                  curr.asVarAny.:=(prev.asValAny)(using dfc.setMetaAnon(alias.meta.position))
                }
              .patch
            if (unique) regPatches += regDinPatch(alias, Patch.Add.Config.Before)
            else regPatches += regDinPatch(posMember, addCfg)
            regsIR
          end addRegs

          nameGroupRegMap.foreach {
            case (NameGroup(groupName, true), groupNamedAliases) =>
              groupNamedAliases.zipWithIndex
                .foreach { case (alias, gnaIdx) =>
                  val namePrefix =
                    if (groupNamedAliases.size == 1) groupName
                    else s"$groupName${(gnaIdx + 1).toPaddedString(groupNamedAliases.size)}"
                  val regsIR = addRegs(List(alias), namePrefix, alias.step, true)
                  regPatches += alias -> Patch.Replace(
                    regsIR.last,
                    Patch.Replace.Config.ChangeRefAndRemove
                  )
                  patchRemoveHistoryInit(alias)
                }
            case (NameGroup(groupName, false), groupNamedAliases) =>
              val regsIR = addRegs(
                groupNamedAliases,
                groupName,
                groupNamedAliases.map(_.getTotalSteps).max,
                false
              )
              groupNamedAliases.foreach { alias =>
                regPatches += alias -> Patch.Replace(
                  regsIR(alias.getTotalSteps - 1),
                  Patch.Replace.Config.ChangeRefAndRemove
                )
                patchRemoveHistoryInit(alias)
              }
          }
        List(
          Some(regDsn.patch),
          regPatches,
          properAliasesPatches
        ).flatten
      case _ => None
    }
    designDB.patch(patchList)
  end transform
end NameRegAliases

extension [T: HasDB](t: T)
  def nameRegAliases(using CompilerOptions): DB =
    StageRunner.run(NameRegAliases)(t.db)
