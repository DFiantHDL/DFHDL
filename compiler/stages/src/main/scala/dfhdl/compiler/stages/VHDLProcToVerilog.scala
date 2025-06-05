package dfhdl.compiler.stages

import dfhdl.compiler.ir.*
import dfhdl.compiler.patching.*
import dfhdl.compiler.analysis.*
import dfhdl.options.CompilerOptions
import DFConditional.{DFIfElseBlock, DFIfHeader}
import DFVal.Func.Op as FuncOp
import ProcessBlock.Sensitivity
import dfhdl.core.DomainType.ED
import dfhdl.compiler.ir.DFVal.PortByNameSelect

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
  def dependencies: List[Stage] = List(DropMagnets)

  def nullifies: Set[Stage] = Set()
  override def runCondition(using co: CompilerOptions): Boolean = co.backend.isVerilog

  def transform(designDB: DB)(using MemberGetSet, CompilerOptions): DB =
    given RefGen = RefGen.fromGetSet
    val patchList: List[(DFMember, Patch)] = designDB.members.flatMap {
      case pb @ ProcessBlock(sensitivity = Sensitivity.List(stRefs)) =>
        def getStVals = stRefs.view.map(_.get)
        val stValsStripped = getStVals.map(_.stripPortSel).toSet
        pb
          .members(MemberView.Folded)
          .collect { case ifBlock: DFIfElseBlock => ifBlock } match
          case ifBlock :: Nil if stValsStripped.size == 1 =>
            ifBlock.guardRef.get match
              case clkEdge @ ClkEdge(clkns @ StrippedPortByNameSelect(clk), edge)
                  if stValsStripped.contains(clk) =>
                val dsn = new MetaDesign(pb, Patch.Add.Config.ReplaceWithLast(), domainType = ED):
                  val clkEdgeSig = edge match
                    case ClkCfg.Edge.Rising  => clk.asValOf[Bit].rising
                    case ClkCfg.Edge.Falling => clk.asValOf[Bit].falling
                  val newPB =
                    dfhdl.core.Process.Block.list(List(clkEdgeSig))(using dfc.setMeta(pb.meta)).asIR
                val pbnsRemoveList = (clkns :: getStVals.toList).collect {
                  case pbns: PortByNameSelect =>
                    pbns -> Patch.Remove()
                }
                pbnsRemoveList ++ List(
                  clkEdge -> Patch.Remove(),
                  ifBlock.prevBlockOrHeaderRef.get -> Patch.Remove(),
                  dsn.patch,
                  ifBlock -> Patch.Replace(dsn.newPB, Patch.Replace.Config.ChangeRefAndRemove)
                )
              case _ => Nil
          case ifBlock :: elseBlock :: Nil
              if stValsStripped.size == 2 && elseBlock.getFirstCB == ifBlock =>
            (ifBlock.guardRef.get, elseBlock.guardRef.get) match
              case (
                    rstActive @ RstActive(StrippedPortByNameSelect(rst), active),
                    clkEdge @ ClkEdge(clkns @ StrippedPortByNameSelect(clk), edge)
                  ) if stValsStripped == Set(clk, rst) =>
                val dsn = new MetaDesign(pb, Patch.Add.Config.ReplaceWithLast(), domainType = ED):
                  val clkEdgeSig = edge match
                    case ClkCfg.Edge.Rising  => clk.asValOf[Bit].rising
                    case ClkCfg.Edge.Falling => clk.asValOf[Bit].falling
                  val rstEdgeSig = active match
                    case RstCfg.Active.High => rst.asValOf[Bit].rising
                    case RstCfg.Active.Low  => rst.asValOf[Bit].falling
                  val newPB =
                    dfhdl.core.Process.Block.list(List(clkEdgeSig, rstEdgeSig))(using
                      dfc.setMeta(pb.meta)
                    ).asIR
                val pbnsRemoveList = (clkns :: getStVals.toList).collect {
                  case pbns: PortByNameSelect =>
                    pbns -> Patch.Remove()
                }
                pbnsRemoveList ++ List(
                  clkEdge -> Patch.Replace(DFMember.Empty, Patch.Replace.Config.ChangeRefAndRemove),
                  dsn.patch
                )
              case _ => Nil
          case _ => None
        end match
      case _ => None
    }
    designDB.patch(patchList)
  end transform
end VHDLProcToVerilog

extension [T: HasDB](t: T)
  def vhdlProcToVerilog(using CompilerOptions): DB =
    StageRunner.run(VHDLProcToVerilog)(t.db)
