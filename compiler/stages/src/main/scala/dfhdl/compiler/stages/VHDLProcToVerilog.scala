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

/** Identity wrapper comparing process clock/reset signals structurally (`=~`), since each read of a
  * port through a design instance mints its own `PortByNameSelect` member. Shared by
  * `VHDLProcToVerilog` and `VerilogProcToVHDL` so both stages recognize the same signals.
  */
private[stages] final case class SensSignal(dfVal: DFVal)(using MemberGetSet, Printer)
    derives CanEqual:
  override def equals(that: Any): Boolean = that match
    case that: SensSignal => that.dfVal =~ dfVal
    case _                => false
  override def hashCode(): Int = dfVal.codeString.hashCode()

//format: off
/** This stage transforms a sequential process from a VHDL style to a Verilog style, where the
  * clock (and reset) events are expressed as edge functions in the sensitivity list instead of
  * guards inside the process.
  *
  * ==Rule 1: Single clock guard==
  * {{{
  * // Before
  * process(clk):
  *   if (clk.rising)
  *     y := x
  *
  * // After
  * process(clk.rising):
  *   y := x
  * }}}
  *
  * ==Rule 2: Async reset, if-reset-else-clock==
  * The clock edge guard of the `else if` branch moves into the sensitivity list, and the reset
  * gains an edge according to its active level:
  * {{{
  * // Before
  * process(clk, rst):
  *   if (rst)
  *     y := 0
  *   else if (clk.rising)
  *     y := x
  *
  * // After
  * process(clk.rising, rst.rising):
  *   if (rst)
  *     y := 0
  *   else
  *     y := x
  * }}}
  *
  * ==Rule 3: Async reset, reset-at-the-end==
  * When the reset condition is a separate `if` statement placed last in the process (overriding
  * the clocked assignments), the clock edge guard is unwrapped and the final reset `if` statement
  * is kept as is:
  * {{{
  * // Before
  * process(clk, rst):
  *   if (clk.rising)
  *     y := x
  *   if (rst)
  *     y := 0
  *
  * // After
  * process(clk.rising, rst.rising):
  *   y := x
  *   if (rst)
  *     y := 0
  * }}}
  */
//format: on
case object VHDLProcToVerilog extends HierarchyStage:
  def dependencies: List[Stage] = List(DropMagnets)

  def nullifies: Set[Stage] = Set()
  override def runCondition(using co: CompilerOptions): Boolean = co.backend.isVerilog

  def transformSubDB(rootDB: DB)(using MemberGetSet, CompilerOptions, RefGen): DB =
    given Printer = DefaultPrinter
    val patches = subDB.members.flatMap {
      case pb @ ProcessBlock(sensitivity = Sensitivity.List(stRefs)) =>
        def getStVals = stRefs.view.map(_.get)
        val stValsStripped = getStVals.map(SensSignal(_)).toSet
        pb
          .members(MemberView.Folded)
          .collect { case ifBlock: DFIfElseBlock => ifBlock } match
          // Rule 1: single clock guard wrapping the process body
          case ifBlock :: Nil if stValsStripped.size == 1 =>
            ifBlock.guardRef.get match
              case clkEdge @ ClkEdge(clk, edge)
                  if stValsStripped.contains(SensSignal(clk)) =>
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
          // Rule 2: async reset in an if-reset-else-clock structure
          case ifBlock :: elseBlock :: Nil
              if stValsStripped.size == 2 && elseBlock.getFirstCB == ifBlock =>
            (ifBlock.guardRef.get, elseBlock.guardRef.get) match
              case (
                    rstActive @ RstActive(rst, active),
                    clkEdge @ ClkEdge(clk, edge)
                  ) if stValsStripped == Set(SensSignal(clk), SensSignal(rst)) =>
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
          // Rule 3: async reset as a final `if (rst)` statement following the clock guard
          case clkIfBlock :: rstIfBlock :: Nil
              if stValsStripped.size == 2 && rstIfBlock.isFirstCB &&
                pb.members(MemberView.Folded).last == rstIfBlock =>
            (clkIfBlock.guardRef.get, rstIfBlock.guardRef.get) match
              case (
                    clkEdge @ ClkEdge(clk, edge),
                    RstActive(rst, active)
                  ) if stValsStripped == Set(SensSignal(clk), SensSignal(rst)) =>
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
                // the clock guard block is unwrapped: its body is re-owned to `newPB` (which
                // also takes over the rest of the process body, including the reset if)
                pbnsRemoveList ++ List(
                  clkEdge -> Patch.Remove(),
                  clkIfBlock.prevBlockOrHeaderRef.get -> Patch.Remove(),
                  dsn.patch,
                  clkIfBlock -> Patch.Replace(dsn.newPB, Patch.Replace.Config.ChangeRefAndRemove)
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
