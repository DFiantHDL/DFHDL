package dfhdl.compiler.stages

import dfhdl.compiler.analysis.*
import dfhdl.compiler.ir.*
import dfhdl.compiler.patching.*
import dfhdl.internals.*
import dfhdl.options.CompilerOptions

case object ExplicitNamedVars extends Stage:
  def dependencies: List[Stage] = List(NamedAnonCondExpr)
  def nullifies: Set[Stage] = Set(DropLocalDcls)

  object WhenHeader extends Patch.Replace.RefFilter:
    def apply(refs: Set[DFRefAny])(using MemberGetSet): Set[DFRefAny] =
      refs.filter {
        case r: DFRef.TwoWayAny =>
          r.get match
            case header: DFConditional.Header =>
              header.originMembers.view
                .collect { case b: DFConditional.Block => b }
                .exists(_.getRefs.exists(_ equals r))
            case _ => false
        case _ => false
      }
  final val WhenNotHeader = !WhenHeader

  extension (ch: DFConditional.Header)
    // recursive call to patch conditional block chains
    private def patchChains(headerVar: DFVal)(using MemberGetSet, RefGen): List[(DFMember, Patch)] =
      val cbChain = getSet.designDB.conditionalChainTable(ch)
      val lastMembers = cbChain.map(_.members(MemberView.Folded).last)
      lastMembers.flatMap {
        case Ident(underlying: DFConditional.Header) =>
          underlying.patchChains(headerVar)
        case m @ Ident(underlying) =>
          val assignDsn = new MetaDesign(
            m,
            Patch.Add.Config.ReplaceWithLast(Patch.Replace.Config.ChangeRefAndRemove)
          ):
            headerVar.asVarAny.:=(underlying.asValAny)(using dfc.setMetaAnon(m.meta.position))
          Some(assignDsn.patch)
        case _ => ??? // not possible
      }
  end extension

  def transform(designDB: DB)(using MemberGetSet, CompilerOptions): DB =
    given RefGen = RefGen.fromGetSet
    val patchList =
      designDB.members.view
        // just named values
        .collect { case dv: DFVal if !dv.isAnonymous => dv }
        .flatMap {
          // ignoring port and variable declarations
          case _: DFVal.Dcl => None
          // ignoring constant declarations (named constants or derived constants)
          case DclConst() => None
          // named if / match expressions will be changed to statements
          case ch: DFConditional.Header =>
            // removing name and type from header
            val updatedCH = ch match
              case mh: DFConditional.DFMatchHeader => mh.copy(dfType = DFUnit).anonymize
              case ih: DFConditional.DFIfHeader    => ih.copy(dfType = DFUnit).anonymize
            // this variable will replace the header as a value
            val dsn = new MetaDesign(
              ch,
              Patch.Add.Config.ReplaceWithLast(Patch.Replace.Config.FullReplacement, WhenHeader)
            ):
              final val plantedNewVar = ch.asValAny.genNewVar(using dfc.setName(ch.getName))
              val newVarIR = plantedNewVar.asIR
              plantMember(updatedCH)
            val chPatchList = List(
              // replacing all the references of header as a conditional header
              dsn.patch,
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
            val dsn = new MetaDesign(named, Patch.Add.Config.ReplaceWithFirst()):
              final val plantedNewVar = named.asValAny.genNewVar(using dfc.setMeta(named.meta))
              plantedNewVar.:=(plantMember(anonIR).asValAny)(using
                dfc.setMetaAnon(named.meta.position)
              )
            List(dsn.patch)
        }
        .toList
    designDB.patch(patchList)
  end transform
end ExplicitNamedVars

//This stage turns all named values to variables that get assigned.
//As a result, conditional expressions (if/match) are converted to statements.
extension [T: HasDB](t: T)
  def explicitNamedVars(using CompilerOptions): DB =
    StageRunner.run(ExplicitNamedVars)(t.db)
