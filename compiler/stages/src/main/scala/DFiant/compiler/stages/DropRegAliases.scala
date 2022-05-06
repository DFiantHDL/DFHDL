package DFiant.compiler.stages

import DFiant.compiler.analysis.*
import DFiant.compiler.ir.DFRef.TwoWay
import DFiant.compiler.ir.{*, given}
import DFiant.compiler.patching.*
import DFiant.internals.*

import scala.annotation.tailrec
import scala.collection.mutable

/** This stage drops register aliases (e.g., `x.reg`) and replaces them with explicit register
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
  *        val x = DFUInt(8) <> WIRE
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
case object DropRegAliases extends Stage:
  def dependencies: List[Stage] = List(DFHDLUniqueNames, SimpleOrderMembers)
  def nullifies: Set[Stage] = Set()
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
        case dcl: DFVal.Dcl if dcl.getAssignmentsTo.size > 1 => NameGroup(s"${dcl.name}_ver", true)
        case dfVal: DFVal if dfVal.isAnonymous =>
          dfVal.suggestName.map(NameGroup(_, true)).getOrElse(NameGroup(dfVal.name, false))
        case dfVal: DFVal => NameGroup(dfVal.name, false)
  end extension

  def transform(designDB: DB)(using MemberGetSet): DB =
    val patchList: List[(DFMember, Patch)] = designDB.namedOwnerMemberList.flatMap {
      case (owner: (DFDomainOwner & DFBlock), members) =>
        val nameGroupRegMap =
          members.view
            .flatMap {
              case regAlias: DFVal.Alias.History =>
                regAlias.op match
                  case _: DFVal.Alias.History.Op.Reg => Some(regAlias)
                  case _                             => None
              case _ => None
            }
            .groupByOrdered(_.getNameGroup)

        // assumes we ordered the members so that declarations come first
        val lastDcl = members.view.takeWhile {
          case _: DFVal.Dcl => true
          case _            => false
        }.last
        val regPatches = mutable.ListBuffer.empty[(DFMember, Patch)]
        val regDsn = new MetaDesign(DFiant.core.DFC.Domain.RT):
          def addRegs(
              alias: DFVal.Alias.History,
              namePrefix: String,
              maxRegs: Int,
              unique: Boolean
          ): List[DFVal] =
            val regs = for (i <- 1 to maxRegs) yield
              val nameSuffix =
                if (maxRegs == 1) "_reg"
                else s"_reg${i.toPaddedString(maxRegs)}"
              val regName =
                if (i == maxRegs && !alias.isAnonymous) alias.name
                else namePrefix + nameSuffix
              import DFiant.core.{DFTypeAny, asFE}
              alias.dfType.asFE[DFTypeAny] <> REG setName regName
            val regsIR = regs.map(_.asIR).toList
            val relVal = alias.getNonRegAliasRelVal
            import DFiant.core.DFVal.Alias.RegDIN
            val regDinDsn = new MetaDesign(DFiant.core.DFC.Domain.RT):
              (relVal :: regsIR).lazyZip(regsIR).foreach { (prev, curr) =>
                RegDIN(curr.asValAny) := prev.asValAny
              }
            if (unique)
              regPatches += alias -> Patch.Add(regDinDsn, Patch.Add.Config.Before)
            else
              regPatches += lastDcl -> Patch.Add(regDinDsn, Patch.Add.Config.After)
            regsIR
          end addRegs

          nameGroupRegMap.foreach {
            case (NameGroup(groupName, true), groupNamedAliases) =>
              groupNamedAliases.zipWithIndex
                .foreach { case (alias, gnaIdx) =>
                  val namePrefix =
                    if (groupNamedAliases.size == 1) groupName
                    else s"$groupName${(gnaIdx + 1).toPaddedString(groupNamedAliases.size)}"
                  val regsIR = addRegs(alias, namePrefix, alias.step, true)
                  regPatches += alias -> Patch.Replace(
                    regsIR.last,
                    Patch.Replace.Config.ChangeRefAndRemove
                  )
                }
            case (NameGroup(groupName, false), groupNamedAliases) =>
              val regsIR = addRegs(
                groupNamedAliases.head,
                groupName,
                groupNamedAliases.map(_.getTotalSteps).max,
                false
              )
              groupNamedAliases.foreach { alias =>
                regPatches += alias -> Patch.Replace(
                  regsIR(alias.getTotalSteps - 1),
                  Patch.Replace.Config.ChangeRefAndRemove
                )
              }
          }
        List(
          Some(lastDcl -> Patch.Add(regDsn, Patch.Add.Config.After)),
          regPatches
        ).flatten
      case _ => None
    }

    designDB.patch(patchList)
  end transform
end DropRegAliases

extension [T: HasDB](t: T) def dropRegAliases: DB = StageRunner.run(DropRegAliases)(t.db)
