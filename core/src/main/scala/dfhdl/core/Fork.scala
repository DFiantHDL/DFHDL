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
    protected type EDDomainOnly[A] = AssertGiven[
      A <:< DomainType.ED,
      "Fork-join is only allowed under event-driven (ED) domains (RT support is planned)."
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
    object forkJoin:
      def apply(block: DFC.Scope.Process ?=> Unit)(using
          dt: DomainType
      )(using EDDomainOnly[dt.type], InProcess, DFC): Unit =
        forkBlock(ir.ForkBlock.Join.All)(block)
    object forkJoinAny:
      def apply(block: DFC.Scope.Process ?=> Unit)(using
          dt: DomainType
      )(using EDDomainOnly[dt.type], InProcess, DFC): Unit =
        forkBlock(ir.ForkBlock.Join.Any)(block)
    object forkJoinNone:
      def apply(block: DFC.Scope.Process ?=> Unit)(using
          dt: DomainType
      )(using EDDomainOnly[dt.type], InProcess, DFC): Unit =
        forkBlock(ir.ForkBlock.Join.None)(block)
  end Ops
end Fork
