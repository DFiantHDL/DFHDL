package DFiant.compiler.stages

import DFiant.compiler.analysis.*
import DFiant.compiler.ir.*
import DFiant.compiler.ir.DFConditional.DFCaseBlock.Pattern
import DFiant.compiler.patching.*
import DFiant.internals.*

private class DropBinds(db: DB) extends Stage(db):
  private object ReplacePattern:
    def unapply(pattern: Pattern): Option[Pattern] =
      pattern match
        case Pattern.CatchAll          => None
        case Pattern.Singleton(token)  => None
        case Pattern.Alternative(list) => None
        case Pattern.Struct(name, fieldPatterns) =>
          var mod = false
          val updatedFieldPatterns = fieldPatterns.map {
            case ReplacePattern(updated) =>
              mod = true
              updated
            case notUpdated => notUpdated
          }
          if (mod) Some(Pattern.Struct(name, updatedFieldPatterns))
          else None
        case Pattern.Bind(ref, pattern) =>
          Some(unapply(pattern).getOrElse(pattern))
        case Pattern.BindSI(op, parts, refs) =>
          val bubbles =
            refs.view.map(_.get.dfType.width).map("?" * _).map { qmarks =>
              op match
                case "b" => qmarks
                case "h" => s"{$qmarks}" // using binary mode for hex
            }
          val tokenStr =
            Seq(parts, bubbles)
              .flatMap(_.zipWithIndex)
              .sortBy(_._2)
              .map(_._1)
              .mkString
          import DFiant.hdl.{b, h}
          val token = op match
            case "b" => b"${tokenStr}"
            case "h" => h"${tokenStr}"
          Some(Pattern.Singleton(token.asIR))
  end ReplacePattern
  override def transform: DB =
//    val binds = designDB.members.collect { case m @ Bind(_) => m }
    val patchList = designDB.conditionalChainTable.flatMap {
      case (mh: DFConditional.DFMatchHeader, cases: List[DFConditional.DFCaseBlock @unchecked]) =>
        cases.view.flatMap(c =>
          c.pattern match
            case ReplacePattern(pattern) =>
              Some(
                c -> Patch.Replace(c.copy(pattern = pattern), Patch.Replace.Config.FullReplacement)
              )
            case _ => None
        )
      case _ => None
    }
    designDB.patch(patchList)
  end transform
end DropBinds

extension [T: HasDB](t: T) def dropBinds: DB = new DropBinds(t.db).transform
