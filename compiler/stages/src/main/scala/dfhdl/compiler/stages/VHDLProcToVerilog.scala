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
import dfhdl.compiler.printing.*

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
case object VHDLProcToVerilog extends HierarchyStage:
  def dependencies: List[Stage] = List(DropMagnets)

  def nullifies: Set[Stage] = Set()
  override def runCondition(using co: CompilerOptions): Boolean = co.backend.isVerilog

  def transformSubDB(rootDB: DB)(using MemberGetSet, CompilerOptions, RefGen): DB =
    given Printer = DefaultPrinter
    case class ConnectionElement(dfVal: DFVal) derives CanEqual:
      override def equals(that: Any): Boolean =
        that.asInstanceOf[ConnectionElement].dfVal =~ dfVal
      override def hashCode(): Int = dfVal.codeString.hashCode()
    val patches = subDB.members.flatMap {
      case pb @ ProcessBlock(sensitivity = Sensitivity.List(stRefs)) =>
        def getStVals = stRefs.view.map(_.get)
        val stValsStripped = getStVals.map(ConnectionElement(_)).toSet
        pb
          .members(MemberView.Folded)
          .collect { case ifBlock: DFIfElseBlock => ifBlock } match
          case ifBlock :: Nil if stValsStripped.size == 1 =>
            ifBlock.guardRef.get match
              case clkEdge @ ClkEdge(clk, edge)
                  if stValsStripped.contains(ConnectionElement(clk)) =>
                val dsn = new MetaDesign(pb, Patch.Add.Config.ReplaceWithLast(), domainType = ED):
                  val clkEdgeSig = edge match
                    case ClkCfg.Edge.Rising  => clk.cloneAnonValueAndDepsHere.asValOf[Bit].rising
                    case ClkCfg.Edge.Falling => clk.cloneAnonValueAndDepsHere.asValOf[Bit].falling
                  val newPB =
                    dfhdl.core.Process.Block.list(List(clkEdgeSig))(using dfc.setMeta(pb.meta)).asIR
                val pbnsRemoveList = (clk :: getStVals.toList).collect {
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
                    rstActive @ RstActive(rst, active),
                    clkEdge @ ClkEdge(clk, edge)
                  ) if stValsStripped == Set(ConnectionElement(clk), ConnectionElement(rst)) =>
                val dsn = new MetaDesign(pb, Patch.Add.Config.ReplaceWithLast(), domainType = ED):
                  val clkEdgeSig = edge match
                    case ClkCfg.Edge.Rising  => clk.cloneAnonValueAndDepsHere.asValOf[Bit].rising
                    case ClkCfg.Edge.Falling => clk.cloneAnonValueAndDepsHere.asValOf[Bit].falling
                  val rstEdgeSig = active match
                    case RstCfg.Active.High => rst.cloneAnonValueAndDepsHere.asValOf[Bit].rising
                    case RstCfg.Active.Low  => rst.cloneAnonValueAndDepsHere.asValOf[Bit].falling
                  val newPB =
                    dfhdl.core.Process.Block.list(List(clkEdgeSig, rstEdgeSig))(using
                      dfc.setMeta(pb.meta)
                    ).asIR
                val pbnsRemoveList = (clk :: getStVals.toList).collect {
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
    subDB.patch(patches)
  end transformSubDB
end VHDLProcToVerilog

extension [T: HasDB](t: T)
  def vhdlProcToVerilog(using CompilerOptions): DB =
    StageRunner.run(VHDLProcToVerilog)(t.db)
