package DFiant.compiler.stages

import DFiant.compiler.analysis.*
import DFiant.compiler.ir.DFRef.TwoWay
import DFiant.compiler.ir.{*, given}
import DFiant.compiler.patching.*
import DFiant.internals.*

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
private class DropRegAliases(db: DB) extends Stage(db):
  // assumes stages: order, uniqueNames
  override def transform: DB =
    val patchList: List[(DFMember, Patch)] = designDB.ownerMemberList.flatMap {
      case (owner: (DFDomainOwner & DFBlock & DFMember.Named), members) =>
        val relValRegMap =
          members
            .collect {
              case regAlias: DFVal.Alias.History if regAlias.op == DFVal.Alias.History.Op.Reg =>
                regAlias
            }
            .view
            .groupByOrdered(_.relValRef.get)
        // assumes we ordered the members so that declarations come first
        val lastDcl = members.view.takeWhile {
          case _: DFVal.Dcl => true
          case _            => false
        }.last
        val regDsn = new MetaDesign:
          relValRegMap.foreach { case (relVal, aliases) =>
            val maxRegs = aliases.map(_.step).max
            val prefix = relVal.getNameOrSuggestion
            for (i <- 1 to maxRegs) do
              val suffix = s"_reg$i"
              val regName = prefix + suffix
              import DFiant.core.{DFTypeAny, asFE}
              import DFiant.core.Modifier.REG as REGFE
              DFiant.core.DFVal.Dcl(relVal.dfType.asFE[DFTypeAny], REGFE)(using
                dfc.setName(regName)
              )
          }
        Some(lastDcl -> Patch.Add(regDsn, Patch.Add.Config.After))
      case _ => None
    }

    designDB.patch(patchList)
  end transform
end DropRegAliases

extension [T: HasDB](t: T) def dropRegAliases: DB = new DropRegAliases(t.db).transform
