package DFiant.core
import DFiant.internals.*
import DFiant.compiler.ir

object Process:
  type Block = DFOwner[ir.ProcessBlock]
  object Block:
    def list(dfVals: List[DFValAny])(using DFC): Block =
      lazy val sl = ir.ProcessBlock.Sensitivity.List(dfVals.map(_.asIR.refTW(block)))
      lazy val block: ir.ProcessBlock =
        ir.ProcessBlock(
          sl,
          dfc.owner.ref,
          dfc.getMeta,
          ir.DFTags.empty
        ).addMember
      block.asFE
    def all(using DFC): Block =
      ir.ProcessBlock(
        ir.ProcessBlock.Sensitivity.All,
        dfc.owner.ref,
        dfc.getMeta,
        ir.DFTags.empty
      ).addMember
        .asFE
  end Block

  object Ops:
    protected type EDDomainOnly[A] = AssertGiven[
      A <:< Container.Domain.ED,
      "Process block are only allowed inside an event-driven (ED) domain."
    ]
    object process:
      def apply(dfVals: DFValAny*)(block: Container.Scope.Process ?=> Unit)(using
          dt: Container.Domain
      )(using EDDomainOnly[dt.type], DFC): Unit =
        val owner = Block.list(dfVals.toList)
        dfc.enterOwner(owner)
        block(using Container.Scope.Process)
        dfc.exitOwner()
      def all(block: Container.Scope.Process ?=> Unit)(using
          dt: Container.Domain
      )(using EDDomainOnly[dt.type], DFC): Unit =
        val owner = Block.all
        dfc.enterOwner(owner)
        block(using Container.Scope.Process)
        dfc.exitOwner()
    end process
  end Ops

end Process
