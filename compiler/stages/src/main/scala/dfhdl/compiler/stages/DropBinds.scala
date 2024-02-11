package dfhdl.compiler.stages

import dfhdl.compiler.analysis.*
import dfhdl.compiler.ir.*
import dfhdl.compiler.ir.DFConditional.DFCaseBlock.Pattern
import dfhdl.compiler.patching.*
import dfhdl.options.CompilerOptions
import dfhdl.internals.*

import scala.collection.mutable

case object DropBinds extends Stage:
  def dependencies: List[Stage] = List()
  def nullifies: Set[Stage] = Set()
  // this unapply matches on bind patterns, strip them of their binds, and returns the binds as a list
  private object ReplacePattern:
    def unapply(
        pattern: Pattern
    )(using MemberGetSet, dfhdl.core.DFC): Option[(Pattern, List[DFVal])] =
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
          val constStr = parts.coalesce(bubbles).mkString
          import dfhdl.core.DFBits.StrInterp.{b, h}
          val dfVal = op match
            case "b" => b"${constStr}"
            case "h" => h"${constStr}"
          Some(dfhdl.core.DFMatch.Pattern.Singleton(dfVal), refs.map(_.get))
        case _ => None
  end ReplacePattern
  def transform(designDB: DB)(using MemberGetSet, CompilerOptions): DB =
    // going through all DFHDL matches
    val patchList = designDB.conditionalChainTable.toList.flatMap {
      case (mh: DFConditional.DFMatchHeader, cases: List[DFConditional.DFCaseBlock @unchecked]) =>
        val bindCaseMap = mutable.Map.empty[DFVal, DFConditional.DFCaseBlock]
        // TODO: casesPatchList is not used!!!!!
        val casesPatchList = mutable.ListBuffer.empty[(DFMember, Patch)]
        // go through all cases, set a patch to replace bind patterns, and memoize the binds and their cases
        val singletonPatternConstsPatch = new MetaDesign(mh, Patch.Add.Config.Before):
          cases.foreach(c =>
            c.pattern match
              case ReplacePattern(pattern, binds) =>
                // memoize binds and their cases
                binds.foreach(b => bindCaseMap += (b -> c))
                // bind-less pattern
                casesPatchList += c -> Patch.Replace(
                  c.copy(pattern = pattern),
                  Patch.Replace.Config.FullReplacement
                )
              case _ => // do nothing
          )
        .patch
        // group similar binds together
        val bindGroups = bindCaseMap.keys.groupByCompare((l, r) => l =~ r, _.getName.hashCode())
        // will memoize the stalled bind variables required to be added in cases where those binds
        // are not used
        val stalled = mutable.Map(cases.map(c => c -> List.empty[DFVal])*)
        // go through all the groups and set a patch to replace the binds with a variable or named alias
        val bindsPatchList: List[(DFMember, Patch)] = bindGroups.flatMap {
          case bg @ ((headBind: DFVal.Alias) :: otherBinds) =>
            // TODO: current feature limitation of prev is that it can only be applied for
            // initialized variables and ports. This means that binds currently cannot have
            // prev dependencies. If we drop this limitation in the future, then this stage will
            // be modified accordingly.
            // a bind group has a prev alias if at least one of its variables has
            val hasPrevAlias = false // bg.exists(_.hasPrevAlias)
            // In case the group has prev alias, then we need to create a new DFHDL variable
            // and assign the underlying bind value to it. If the bind group contains more than one bind,
            // then the rest of the binds are removed and reference the bind variable we created.
            if (hasPrevAlias)
              val relValIR = headBind.relValRef.get
              val dsn = new MetaDesign(
                headBind,
                Patch.Add.Config.ReplaceWithFirst(Patch.Replace.Config.FullReplacement)
              ):
                val bindVar = headBind.asValAny.genNewVar(using dfc.setName(headBind.getName))
                val bindVarIR = bindVar.asIR
                bindVar := relValIR.asValAny
              val bindVarIR = dsn.bindVarIR
              val coveredCases = bg.map(bindCaseMap(_)).toSet
              val missingCases = cases.filterNot(coveredCases.contains)
              missingCases.foreach(c => stalled += c -> (bindVarIR :: stalled(c)))
              dsn.patch :: otherBinds.map(b =>
                b -> Patch.Replace(bindVarIR, Patch.Replace.Config.ChangeRefAndRemove)
              )
            // In case the group has no prev alias, then all we need is to strip the bind tag from its alias.
            // If the bind group contains more than one bind, then the rest of the binds are removed and
            // reference the first bind that is stripped from its alias.
            else
              val aliasIR = headBind.removeTagOf[Pattern.Bind.Tag.type]
              singletonPatternConstsPatch :: (
                headBind -> Patch.Replace(
                  aliasIR,
                  Patch.Replace.Config.FullReplacement
                )
              ) :: otherBinds.map(b =>
                b -> Patch.Replace(aliasIR, Patch.Replace.Config.ChangeRefAndRemove)
              )
            end if
          case _ => ??? // not possible
        }.toList
        // for all the memoized stalled bind variables and cases we need to populate
        // the missing bind cases with self-assignment of the previous value for each bind.
//        val stallsPatchList: List[(DFMember, Patch)] = stalled.collect {
//          case (c, varsIR) if varsIR.nonEmpty =>
//            val dsn = new MetaDesign():
//              varsIR.view.reverse.forconstStr> v.asVarAny := v.asVarAny.asInitialized.prev)
//            c -> Patch.Add(dsn, Patch.Add.Config.InsideLast)
//        }.toList
        casesPatchList ++ bindsPatchList // ++ stallsPatchList
      case _ => None
    }
    designDB.patch(patchList)
  end transform
end DropBinds

// Drops match case bind values and replaces them with either a variable
// that is assigned with the bind alias or just the named bind alias.
// UniqueDesign stage must be applied after this stage to resolve naming collisions.
extension [T: HasDB](t: T)
  def dropBinds(using CompilerOptions): DB = StageRunner.run(DropBinds)(t.db)
