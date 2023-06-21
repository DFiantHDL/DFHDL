package dfhdl.compiler.stages

import dfhdl.compiler.ir.*
import dfhdl.compiler.patching.*
import dfhdl.compiler.analysis.*
import DFConditional.{DFIfElseBlock, DFIfHeader}
import DFVal.Func.Op as FuncOp
import ProcessBlock.Sensitivity

/** This stage transforms a sequential process from a VHDL style to Verilog style. E.g.,
  * {{{
  *   process(clk):
  *     if (clk.rising)
  *       ....
  * }}}
  * is transformed into
  * {{{
  *   process(clk.rising):
  *     ....
  * }}}
  */
case object VHDLProcToVerilog extends Stage:
  def dependencies: List[Stage] = List()

  def nullifies: Set[Stage] = Set()

  def transform(designDB: DB)(using MemberGetSet): DB =
    val patchList: List[(DFMember, Patch)] = designDB.members.flatMap {
      case pb @ ProcessBlock(Sensitivity.List(stRefs), _, _, _) =>
        val stVals = stRefs.view.map(_.get).toSet
        pb
          .members(MemberView.Folded)
          .collect { case ifBlock: DFIfElseBlock => ifBlock } match
          case ifBlock :: Nil if stVals.size == 1 =>
            ifBlock.guardRef.get match
              case clkEdge @ Edge(edgeVal) if stVals.contains(edgeVal) =>
                val dsn = new MetaDesign(dfhdl.core.DFC.Domain.ED):
                  val clkGuard = clkEdge.op match
                    case FuncOp.rising  => edgeVal.asValOf[Bit].rising
                    case FuncOp.falling => edgeVal.asValOf[Bit].falling
                    case _              => ???
                  val newPB =
                    dfhdl.core.Process.Block.list(List(clkGuard))(using dfc.setMeta(pb.meta)).asIR
                List(
                  clkEdge -> Patch.Remove,
                  ifBlock.prevBlockOrHeaderRef.get -> Patch.Remove,
                  pb -> Patch.Add(dsn, Patch.Add.Config.ReplaceWithLast()),
                  ifBlock -> Patch.Replace(dsn.newPB, Patch.Replace.Config.ChangeRefAndRemove)
                )
              case _ => None
//          case ifBlock :: elseBlock :: Nil if stVals.size == 2 && elseBlock.getFirstCB == ifBlock =>
//            elseBlock.guardRef.get match
//              case clkEdge @ Edge(edgeVal) if stVals.contains(edgeVal) =>
//                val dsn = new MetaDesign(dfhdl.DFC.Domain.ED):
//                  val clkGuard = clkEdge.op match
//                    case FuncOp.rising  => edgeVal.asValOf[Bit].rising
//                    case FuncOp.falling => edgeVal.asValOf[Bit].falling
//                    case _              => ???
//                  val newPB =
//                    dfhdl.core.Process.Block.list(List(clkGuard))(using dfc.setMeta(pb.meta)).asIR
//                List(
//                  clkEdge -> Patch.Remove,
//                  ifBlock.prevBlockOrHeaderRef.get -> Patch.Remove,
//                  pb -> Patch.Add(dsn, Patch.Add.Config.ReplaceWithLast()),
//                  ifBlock -> Patch.Replace(dsn.newPB, Patch.Replace.Config.ChangeRefAndRemove)
//                )
//              case _ => None
          case _ => None
        end match
      case _ => None
    }
    designDB.patch(patchList)
  end transform
end VHDLProcToVerilog

extension [T: HasDB](t: T)
  def vhdlProcToVerilog: DB =
    StageRunner.run(VHDLProcToVerilog)(t.db)(using dfhdl.options.CompilerOptions.default)
