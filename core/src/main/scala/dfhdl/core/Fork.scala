package dfhdl.core
import dfhdl.internals.*
import dfhdl.compiler.ir

object Fork:
  type Block = DFOwner[ir.ForkBlock]
  object Block:
    def apply(join: ir.ForkBlock.Join)(using DFC): Block =
      ir.ForkBlock(
        join,
        dfc.owner.ref,
        dfc.getMeta,
        dfc.tags
      ).addMember.asFE
  end Block

  object Ops:
    // forkJoinAny / forkJoinNone can dynamically spawn threads (e.g. a fork inside a loop), which
    // cannot be represented as a static set of synthesized processes. For now they are restricted
    // to event-driven (ED) domains, where they map to native SystemVerilog `join_any` / `join_none`.
    protected type EDDomainOnly[A] = AssertGiven[
      A <:< DomainType.ED,
      "forkJoinAny and forkJoinNone are only allowed under event-driven (ED) domains."
    ]
    // forkJoin (join-all) blocks the parent until all branches complete, so it is safe in any
    // non-dataflow domain (including register-transfer, where it lowers to a clocked FSM).
    protected type NotDFDomain[A] = AssertGiven[
      util.NotGiven[A <:< DomainType.DF],
      "Fork-join is not supported under dataflow (DF) domains."
    ]
    protected type InProcess = AssertGiven[
      DFC.Scope.Process,
      "Fork-join must be placed inside a process."
    ]
    private def forkBlock(join: ir.ForkBlock.Join)(block: DFC.Scope.Process ?=> Unit)(using
        DFC
    ): Unit =
      val owner = Block(join)
      dfc.enterOwner(owner)
      block(using DFC.Scope.Process)
      dfc.exitOwner()
    def forkJoin(block: DFC.Scope.Process ?=> Unit)(using
        dt: DomainType
    )(using NotDFDomain[dt.type], InProcess, DFC): Unit =
      forkBlock(ir.ForkBlock.Join.All)(block)
    def forkJoinAny(block: DFC.Scope.Process ?=> Unit)(using
        dt: DomainType
    )(using EDDomainOnly[dt.type], InProcess, DFC): Unit =
      forkBlock(ir.ForkBlock.Join.Any)(block)
    def forkJoinNone(block: DFC.Scope.Process ?=> Unit)(using
        dt: DomainType
    )(using EDDomainOnly[dt.type], InProcess, DFC): Unit =
      forkBlock(ir.ForkBlock.Join.None)(block)
  end Ops
end Fork
