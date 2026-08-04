package dfhdl.compiler.stages

import dfhdl.compiler.ir.*
import dfhdl.compiler.patching.*
import dfhdl.compiler.analysis.*
import dfhdl.options.CompilerOptions
import DFConditional.{DFIfElseBlock, DFIfHeader}
import ProcessBlock.Sensitivity
import dfhdl.core.DomainType.ED
import dfhdl.compiler.ir.DFVal.PortByNameSelect
import dfhdl.compiler.printing.*

//format: off
/** This stage transforms a sequential process from a Verilog style to a VHDL style, since a VHDL
  * sensitivity list may only contain signal names (an edge expression like `rising_edge(clk)` is
  * illegal there). It only applies to processes whose sensitivity list is composed entirely of
  * edge functions over simple signals.
  *
  * ==Rule 1: Single edge sensitivity==
  * The entire process body is wrapped by a clock edge guard:
  * {{{
  * // Before
  * process(clk.rising):
  *   y := x
  *
  * // After
  * process(clk):
  *   if (clk.rising)
  *     y := x
  * }}}
  *
  * ==Rule 2: Async reset, if-reset-else-clock==
  * The guard-less `else` branch receives the clock edge guard (the reset edge is implied by the
  * reset condition, so it is dropped from the sensitivity list):
  * {{{
  * // Before
  * process(clk.rising, rst.rising):
  *   if (rst)
  *     y := 0
  *   else
  *     y := x
  *
  * // After
  * process(clk, rst):
  *   if (rst)
  *     y := 0
  *   else if (clk.rising)
  *     y := x
  * }}}
  *
  * ==Rule 3: Async reset, reset-at-the-end==
  * When the reset condition is a separate `if` statement placed last in the process (overriding
  * the clocked assignments), the clocked statements are wrapped by a clock edge guard while the
  * final reset `if` statement is kept as is:
  * {{{
  * // Before
  * process(clk.rising, rst.rising):
  *   y := x
  *   if (rst)
  *     y := 0
  *
  * // After
  * process(clk, rst):
  *   if (clk.rising)
  *     y := x
  *   if (rst)
  *     y := 0
  * }}}
  */
//format: on
case object VerilogProcToVHDL extends HierarchyStage:
  def dependencies: List[Stage] = List(DropMagnets)

  def nullifies: Set[Stage] = Set()
  override def runCondition(using co: CompilerOptions): Boolean = co.backend.isVHDL

  def transformSubDB(rootDB: DB)(using MemberGetSet, CompilerOptions, RefGen): DB =
    given Printer = DefaultPrinter
    val patches = subDB.members.flatMap {
      case pb @ ProcessBlock(sensitivity = Sensitivity.List(stRefs)) =>
        // relevant only when every sensitivity element is an edge function over a simple signal
        val sensEdges = stRefs.flatMap { stRef =>
          stRef.get match
            case edgeFunc: DFVal.Func =>
              edgeFunc match
                case ClkEdge(sig: (DFVal.Dcl | PortByNameSelect), edge) =>
                  Some((edgeFunc, sig, edge))
                case _ => None
            case _ => None
        }
        // the old sensitivity edge functions (and their port selections) are all
        // replaced by fresh signal references, so they are removed
        def sensRemoveList = sensEdges.flatMap { (edgeFunc, sig, _) =>
          (edgeFunc -> Patch.Remove()) ::
            (sig match
              case pbns: PortByNameSelect => List(pbns -> Patch.Remove())
              case _                      => Nil)
        }
        // splits the sensitivity edges into the clock edge and the reset signal, according
        // to the reset signal the reset guard references
        def splitClkRst(rstGuardSig: DFVal): Option[((DFVal, ClkCfg.Edge), DFVal)] =
          val rstId = SensSignal(rstGuardSig)
          sensEdges.partition((_, sig, _) => SensSignal(sig) == rstId) match
            case (List((_, rstSig, _)), List((_, clkSig, clkEdge))) =>
              Some(((clkSig, clkEdge), rstSig))
            case _ => None
        if (sensEdges.length != stRefs.length || sensEdges.isEmpty) None
        else
          sensEdges match
            // Rule 1: single clock => the entire process body is wrapped by an edge guard
            case List((_, clkSig, clkEdge)) =>
              val dsn = new MetaDesign(pb, Patch.Add.Config.ReplaceWithLast(), domainType = ED):
                import dfhdl.core.{DFIf, DFUnit}
                val newPB = dfhdl.core.Process.Block.list(
                  List(clkSig.cloneAnonValueAndDepsHere.asValAny)
                )(using dfc.setMeta(pb.meta))
                dfc.enterOwner(newPB)
                val clkEdgeSig = clkEdge match
                  case ClkCfg.Edge.Rising  => clkSig.cloneAnonValueAndDepsHere.asValOf[Bit].rising
                  case ClkCfg.Edge.Falling => clkSig.cloneAnonValueAndDepsHere.asValOf[Bit].falling
                val clkGuardBlock = DFIf.Block(Some(clkEdgeSig), DFIf.Header(DFUnit))
                dfc.exitOwner()
              // the guard block is the last meta member, so the process body is re-owned
              // into it, while the process itself is replaced in position by `newPB`
              sensRemoveList :+ dsn.patch
            case List(e1, e2) if SensSignal(e1._2) != SensSignal(e2._2) =>
              pb.members(MemberView.Folded).collect { case b: DFIfElseBlock => b } match
                // Rule 2: async reset in an if-reset-else-clock structure => the plain
                // `else` branch becomes an `else if (clk.<edge>)` branch
                case rstIfBlock :: elseBlock :: Nil
                    if elseBlock.getFirstCB == rstIfBlock &&
                      elseBlock.guardRef.get == DFMember.Empty =>
                  rstIfBlock.guardRef.get match
                    case RstActive(rstGuardSig, _) =>
                      splitClkRst(rstGuardSig) match
                        case Some(((clkSig, clkEdge), rstSig)) =>
                          val dsn =
                            new MetaDesign(pb, Patch.Add.Config.ReplaceWithLast(), domainType = ED):
                              val newPB = dfhdl.core.Process.Block.list(
                                List(
                                  clkSig.cloneAnonValueAndDepsHere.asValAny,
                                  rstSig.cloneAnonValueAndDepsHere.asValAny
                                )
                              )(using dfc.setMeta(pb.meta))
                          // the clock edge guard for the `else` branch, physically placed
                          // between the reset branch and the `else` branch (its ownership
                          // reference resolves to `newPB` since `dsn` replaces `pb` first
                          // in the patch list)
                          val guardDsn =
                            new MetaDesign(rstIfBlock, Patch.Add.Config.After, domainType = ED):
                              import dfhdl.core.refTW
                              val clkEdgeSig = clkEdge match
                                case ClkCfg.Edge.Rising =>
                                  clkSig.cloneAnonValueAndDepsHere.asValOf[Bit].rising
                                case ClkCfg.Edge.Falling =>
                                  clkSig.cloneAnonValueAndDepsHere.asValOf[Bit].falling
                              val newGuardRef: DFConditional.Block.GuardRef =
                                clkEdgeSig.asIR.refTW[DFIfElseBlock]
                          sensRemoveList ++ List(
                            dsn.patch,
                            guardDsn.patch,
                            elseBlock -> Patch.Replace(
                              elseBlock.copy(guardRef = guardDsn.newGuardRef),
                              Patch.Replace.Config.FullReplacement
                            )
                          )
                        case None => None
                    case _ => None
                // Rule 3: async reset as a final `if (rst)` statement => the clocked
                // statements are wrapped by an edge guard and the reset if is kept as is
                case rstIfBlock :: Nil
                    if rstIfBlock.isFirstCB && pb.members(MemberView.Folded).last == rstIfBlock =>
                  rstIfBlock.guardRef.get match
                    case rstGuard @ RstActive(rstGuardSig, _) =>
                      splitClkRst(rstGuardSig) match
                        case Some(((clkSig, clkEdge), rstSig)) =>
                          val dsn =
                            new MetaDesign(pb, Patch.Add.Config.ReplaceWithLast(), domainType = ED):
                              import dfhdl.core.{DFIf, DFUnit}
                              val newPB = dfhdl.core.Process.Block.list(
                                List(
                                  clkSig.cloneAnonValueAndDepsHere.asValAny,
                                  rstSig.cloneAnonValueAndDepsHere.asValAny
                                )
                              )(using dfc.setMeta(pb.meta))
                              dfc.enterOwner(newPB)
                              val clkEdgeSig = clkEdge match
                                case ClkCfg.Edge.Rising =>
                                  clkSig.cloneAnonValueAndDepsHere.asValOf[Bit].rising
                                case ClkCfg.Edge.Falling =>
                                  clkSig.cloneAnonValueAndDepsHere.asValOf[Bit].falling
                              val clkGuardBlock = DFIf.Block(Some(clkEdgeSig), DFIf.Header(DFUnit))
                              dfc.exitOwner()
                              val newPBIR = newPB.asIR
                          // the guard block is the last meta member, so the whole process body
                          // is re-owned into it; the final reset if statement (with its header
                          // and guard cone) is then re-owned back to the process level
                          val processLevelKeep =
                            rstGuard.collectRelMembers(false).filter(_.getOwner == pb) ++
                              List(rstIfBlock.prevBlockOrHeaderRef.get, rstIfBlock)
                          sensRemoveList ++
                            (dsn.patch ::
                              processLevelKeep.map(m => m -> Patch.ChangeOwner(dsn.newPBIR)))
                        case None => None
                    case _ => None
                case _ => None
              end match
            case _ => None
          end match
        end if
      case _ => None
    }
    subDB.patch(patches)
  end transformSubDB
end VerilogProcToVHDL

extension [T: HasDB](t: T)
  def verilogProcToVHDL(using CompilerOptions): DB =
    StageRunner.run(VerilogProcToVHDL)(t.db)
