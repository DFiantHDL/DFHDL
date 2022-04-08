package DFiant.compiler.stages

import DFiant.compiler.analysis.*
import DFiant.compiler.ir.DFRef.TwoWay
import DFiant.compiler.ir.{*, given}
import DFiant.compiler.patching.*
import DFiant.internals.*

import scala.collection.mutable

private class DropRegAliases(db: DB) extends Stage(db):
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
            val prefix = relVal.name
            for (i <- 1 to maxRegs) do
              val suffix = s"_reg$i"
              val regName = prefix + suffix
              relVal.asValAny.genNewVar(using dfc.setName(regName))
          }
        Some(lastDcl -> Patch.Add(regDsn, Patch.Add.Config.After))
      case _ => None
    }

    designDB.patch(patchList)
  end transform
end DropRegAliases

extension [T: HasDB](t: T) def dropRegAliases: DB = new DropRegAliases(t.db).transform
