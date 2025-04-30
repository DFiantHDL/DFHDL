package dfhdl.compiler.stages

import dfhdl.compiler.analysis.*
import dfhdl.compiler.ir.*
import dfhdl.compiler.patching.*
import dfhdl.internals.*
import dfhdl.options.CompilerOptions

/* This stage transforms an assignment from a conditional expression to a statement.*/
case object ExplicitCondExprAssign extends Stage:
  def dependencies: List[Stage] = List()
  def nullifies: Set[Stage] = Set(DropUnreferencedAnons)

  def transform(designDB: DB)(using MemberGetSet, CompilerOptions): DB =
    extension (ch: DFConditional.Header)
      // recursive call to patch conditional block chains
      private def patchChains(headerVar: DFVal, op: DFNet.Op): List[(DFMember, Patch)] =
        // changing type of header to unit, since the expression is now a statement
        val headerPatch = ch -> Patch.Replace(
          ch.updateDFType(DFUnit),
          Patch.Replace.Config.FullReplacement
        )
        val cbChain = getSet.designDB.conditionalChainTable(ch)
        val lastMembers = cbChain.map(_.members(MemberView.Folded).last)
        headerPatch :: lastMembers.flatMap {
          case ident @ Ident(underlying: DFConditional.Header) =>
            ident -> Patch.Remove() :: underlying.patchChains(headerVar, op)
          case ident @ Ident(underlying) =>
            val assignDsn = new MetaDesign(
              ident,
              Patch.Add.Config.ReplaceWithLast(Patch.Replace.Config.FullReplacement)
            ):
              (op: @unchecked) match
                case DFNet.Op.Assignment =>
                  headerVar.asVarAny.:=(underlying.asValAny)(using
                    dfc.setMetaAnon(ident.meta.position)
                  )
                case DFNet.Op.NBAssignment =>
                  import dfhdl.core.nbassign
                  headerVar.asVarAny.nbassign(underlying.asValAny)(using
                    dfc.setMetaAnon(ident.meta.position)
                  )
            Some(assignDsn.patch)
          case _ => ??? // not possible
        }
      end patchChains
      private def patchChainsNet(
          headerVar: DFVal,
          net: DFNet,
          op: DFNet.Op
      ): List[(DFMember, Patch)] =
        val removeNetPatch = net -> Patch.Remove()
        removeNetPatch :: ch.patchChains(headerVar, op)
    end extension
    val patchList = designDB.members.view
      // collect all the assignments from anonymous conditionals
      .flatMap {
        case net @ DFNet.Assignment(toVal, header: DFConditional.Header) if header.isAnonymous =>
          header.patchChainsNet(toVal, net, net.op)
        case net @ DFNet.Connection(toVal: DFVal, header: DFConditional.Header, _)
            if !net.isViaConnection && header.isAnonymous && (toVal.isPortOut || toVal.isVar) =>
          header.patchChainsNet(toVal, net, DFNet.Op.Assignment)
        case _ => Nil
      }.toList
    designDB.patch(patchList)
  end transform
end ExplicitCondExprAssign

extension [T: HasDB](t: T)
  def explicitCondExprAssign(using CompilerOptions): DB =
    StageRunner.run(ExplicitCondExprAssign)(t.db)
