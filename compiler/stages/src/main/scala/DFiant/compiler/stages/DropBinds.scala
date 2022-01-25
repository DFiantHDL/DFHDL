package DFiant.compiler.stages

import DFiant.compiler.analysis.*
import DFiant.compiler.ir.*
import DFiant.compiler.ir.DFConditional.DFCaseBlock.Pattern
import DFiant.compiler.patching.*
import DFiant.internals.*

import scala.collection.mutable

private class DropBinds(db: DB) extends Stage(db):
  private object ReplacePattern:
    def unapply(pattern: Pattern): Option[(Pattern, List[DFVal])] =
      pattern match
        case Pattern.Struct(name, fieldPatterns) =>
          var bindVals = List.empty[DFVal]
          val updatedFieldPatterns = fieldPatterns.map {
            case ReplacePattern(updated, binds) =>
              bindVals = binds ++ bindVals
              updated
            case notUpdated => notUpdated
          }
          if (bindVals.nonEmpty) Some(Pattern.Struct(name, updatedFieldPatterns), bindVals)
          else None
        case Pattern.Bind(ref, pattern) =>
          pattern match
            case ReplacePattern(updated, binds) => Some(updated, ref.get :: binds)
            case _                              => Some(pattern, ref.get :: Nil)
        case Pattern.BindSI(op, parts, refs) =>
          val bubbles =
            refs.view.map(_.get.dfType.width).map("?" * _).map { qmarks =>
              op match
                case "b" => qmarks
                case "h" => s"{$qmarks}" // using binary mode for hex
            }
          val tokenStr = parts.coalesce(bubbles).mkString
          import DFiant.hdl.{b, h}
          val token = op match
            case "b" => b"${tokenStr}"
            case "h" => h"${tokenStr}"
          Some(Pattern.Singleton(token.asIR), refs.map(_.get))
        case _ => None
  end ReplacePattern
  override def transform: DB =
    val patchList = designDB.conditionalChainTable.flatMap {
      case (mh: DFConditional.DFMatchHeader, cases: List[DFConditional.DFCaseBlock @unchecked]) =>
        val matchBinds = mutable.Map.empty[DFVal, DFConditional.DFCaseBlock]
        val casesPatchList: List[(DFMember, Patch)] = cases.flatMap(c =>
          c.pattern match
            case ReplacePattern(pattern, binds) =>
              binds.foreach(b => matchBinds += (b -> c))
              Some(
                c -> Patch.Replace(c.copy(pattern = pattern), Patch.Replace.Config.FullReplacement)
              )
            case _ => None
        )
        val bindGroups = matchBinds.keys.groupByCompare((l, r) => l =~ r, _.name.hashCode())
        val stalled = mutable.Map(cases.map(c => c -> List.empty[DFVal])*)
        val bindsPatchList: List[(DFMember, Patch)] = bindGroups.flatMap {
          case bg @ ((headBind: DFVal.Alias) :: otherBinds) =>
            val hasPrevAlias = bg.exists(_.hasPrevAlias)
            if (hasPrevAlias)
              val coveredCases = bg.map(matchBinds(_)).toSet
              val missingCases = cases.filterNot(coveredCases.contains)
              val relValIR = headBind.relValRef.get
              val dsn = new MetaDesign:
                val bindVar = headBind.asValAny.genNewVar(using dfc.setName(headBind.name))
                val bindVarIR = bindVar.asIR
                bindVar := relValIR.asValAny
              val bindVarIR = dsn.bindVarIR
              missingCases.foreach(c => stalled += c -> (bindVarIR :: stalled(c)))
              headBind -> Patch.Add(
                dsn,
                Patch.Add.Config.ReplaceWithFirst(Patch.Replace.Config.FullReplacement)
              ) :: otherBinds.map(b =>
                b -> Patch.Replace(bindVarIR, Patch.Replace.Config.ChangeRefAndRemove)
              )
            else
              val aliasIR = headBind.removeTagOf[Pattern.Bind.Tag.type]
              (headBind -> Patch.Replace(
                aliasIR,
                Patch.Replace.Config.FullReplacement
              )) :: otherBinds.map(b =>
                b -> Patch.Replace(aliasIR, Patch.Replace.Config.ChangeRefAndRemove)
              )
            end if
          case _ => ??? // not possible
        }.toList
        val stallsPatchList = stalled.collect {
          case (c, varsIR) if varsIR.nonEmpty =>
            val dsn = new MetaDesign:
              varsIR.view.reverse.foreach(v => v.asVarAny := v.asValAny.prev)
            c -> Patch.Add(dsn, Patch.Add.Config.InsideLast)
        }
        casesPatchList ++ bindsPatchList ++ stallsPatchList
      case _ => None
    }
    designDB.patch(patchList)
  end transform
end DropBinds

extension [T: HasDB](t: T) def dropBinds: DB = new DropBinds(t.db).transform
