package DFiant.compiler.stages

import DFiant.compiler.analysis.*
import DFiant.compiler.ir.*
import DFiant.compiler.patching.*
import DFiant.internals.*

import scala.reflect.classTag

private class ExplicitNamedVars(db: DB) extends Stage(db):

  object WhenHeader extends Patch.Replace.RefFilter:
    val ifHeaderTag = classTag[DFConditional.DFIfHeader]
    val matchHeaderTag = classTag[DFConditional.DFMatchHeader]
    def apply(refs: Set[DFRefAny])(using MemberGetSet): Set[DFRefAny] =
      refs.filter { r => (r.refType equals ifHeaderTag) || (r.refType equals matchHeaderTag) }
  object WhenNotHeader extends Patch.Replace.RefFilter:
    def apply(refs: Set[DFRefAny])(using MemberGetSet): Set[DFRefAny] =
      refs -- WhenHeader(refs)

  override def transform: DB =
    val patchList =
      designDB.members.view
        // just named values
        .collect { case dv: DFVal if !dv.isAnonymous => dv }
        .flatMap {
          // ignoring ports and variables
          case _: DFVal.Dcl => None
          // ignoring constants
          case _: DFVal.Const => None // named constants remain as they are
          // named if / match expressions will be changed to statements
          case ch: DFConditional.Header =>
            val updatedCH = ch match
              case mh: DFConditional.DFMatchHeader => mh.copy(dfType = NoType).anonymize
              case ih: DFConditional.DFIfHeader =>
                ih.copy(dfType = NoType).anonymize
            val dsn = new MetaDesign:
              final val plantedNewVar = ch.asValAny.genNewVar(using dfc.setName(ch.name))
              val newVarIR = plantedNewVar.asIR
              plantMember(updatedCH)
            val chPatchList = List(
              ch -> Patch.Add(
                dsn,
                Patch.Add.Config.ReplaceWithLast(Patch.Replace.Config.FullReplacement, WhenHeader)
              ),
              ch -> Patch.Replace(
                dsn.newVarIR,
                Patch.Replace.Config.ChangeRefOnly,
                WhenNotHeader
              )
            )
            val cbChain = designDB.conditionalChainTable(ch)
            val lastMembers = cbChain.map(_.members(MemberView.Folded).last)
            val cbPatchList = lastMembers.collect { case m @ Ident(underlying) =>
              val assignDsn = new MetaDesign:
                dsn.newVarIR.asVarAny := underlying.asValAny
              m -> Patch.Add(
                assignDsn,
                Patch.Add.Config.ReplaceWithLast(Patch.Replace.Config.ChangeRefAndRemove)
              )
            }
            chPatchList ++ cbPatchList
          case named =>
            val anonIR = named.anonymize
            val dsn = new MetaDesign:
              final val plantedNewVar = named.asValAny.genNewVar(using dfc.setName(named.name))
              plantedNewVar := plantMember(anonIR).asValAny
            List(named -> Patch.Add(dsn, Patch.Add.Config.ReplaceWithFirst()))
        }
        .toList
    designDB.patch(patchList)
  end transform
end ExplicitNamedVars

//This stage turns all named values to variables that get assigned
extension [T: HasDB](t: T) def explicitNamedVars: DB = new ExplicitNamedVars(t.db).transform
