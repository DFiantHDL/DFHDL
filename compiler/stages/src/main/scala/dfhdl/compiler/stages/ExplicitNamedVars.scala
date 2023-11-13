package dfhdl.compiler.stages

import dfhdl.compiler.analysis.*
import dfhdl.compiler.ir.*
import dfhdl.compiler.patching.*
import dfhdl.internals.*
import dfhdl.options.CompilerOptions

import scala.reflect.classTag

case object ExplicitNamedVars extends Stage:
  def dependencies: List[Stage] = Nil
  def nullifies: Set[Stage] = Set(DropLocalDcls, DropCondDcls)

  object WhenHeader extends Patch.Replace.RefFilter:
    val ifHeaderTag = classTag[DFConditional.DFIfHeader]
    val matchHeaderTag = classTag[DFConditional.DFMatchHeader]
    def apply(refs: Set[DFRefAny])(using MemberGetSet): Set[DFRefAny] =
      refs.filter { r => (r.refType equals ifHeaderTag) || (r.refType equals matchHeaderTag) }
  final val WhenNotHeader = !WhenHeader

  extension (ch: DFConditional.Header)
    // recursive call to patch conditional block chains
    private def patchChains(headerVar: DFVal)(using MemberGetSet): List[(DFMember, Patch)] =
      val cbChain = getSet.designDB.conditionalChainTable(ch)
      val lastMembers = cbChain.map(_.members(MemberView.Folded).last)
      lastMembers.flatMap {
        case Ident(underlying: DFConditional.Header) =>
          underlying.patchChains(headerVar)
        case m @ Ident(underlying) =>
          val assignDsn = new MetaDesign():
            headerVar.asVarAny := underlying.asValAny
          Some(
            m -> Patch.Add(
              assignDsn,
              Patch.Add.Config.ReplaceWithLast(Patch.Replace.Config.ChangeRefAndRemove)
            )
          )
        case _ => ??? // not possible
      }

  def transform(designDB: DB)(using MemberGetSet, CompilerOptions): DB =
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
            // removing name and type from header
            val updatedCH = ch match
              case mh: DFConditional.DFMatchHeader => mh.copy(dfType = NoType).anonymize
              case ih: DFConditional.DFIfHeader    => ih.copy(dfType = NoType).anonymize
            // this variable will replace the header as a value
            val dsn = new MetaDesign():
              final val plantedNewVar = ch.asValAny.genNewVar(using dfc.setName(ch.getName))
              val newVarIR = plantedNewVar.asIR
              plantMember(updatedCH)
            val chPatchList = List(
              // replacing all the references of header as a conditional header
              ch -> Patch.Add(
                dsn,
                Patch.Add.Config.ReplaceWithLast(Patch.Replace.Config.FullReplacement, WhenHeader)
              ),
              // replacing all the references of header as a value
              ch -> Patch.Replace(
                dsn.newVarIR,
                Patch.Replace.Config.ChangeRefOnly,
                WhenNotHeader
              )
            )
            chPatchList ++ ch.patchChains(dsn.newVarIR)
          // all other named values
          case named =>
            val anonIR = named.anonymize
            val dsn = new MetaDesign():
              final val plantedNewVar = named.asValAny.genNewVar(using
                dfc.setName(named.getName).setAnnotations(named.meta.annotations)
              )
              plantedNewVar := plantMember(anonIR).asValAny
            List(named -> Patch.Add(dsn, Patch.Add.Config.ReplaceWithFirst()))
        }
        .toList
    designDB.patch(patchList)
  end transform
end ExplicitNamedVars

//This stage turns all named values to variables that get assigned.
//As a result, conditional expressions (if/match) are converted to statements.
extension [T: HasDB](t: T)
  def explicitNamedVars: DB =
    StageRunner.run(ExplicitNamedVars)(t.db)(using dfhdl.options.CompilerOptions.default)
