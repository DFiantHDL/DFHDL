package dfhdl.core
import dfhdl.internals.*
import dfhdl.compiler.ir

object Process:
  type Block = DFOwner[ir.ProcessBlock]
  object Block:
    def list(dfVals: List[DFValAny])(using DFC): Block =
      val sl = ir.ProcessBlock.Sensitivity.List(dfVals.map(_.asIR.refTW[ir.ProcessBlock]))
      val block: ir.ProcessBlock =
        ir.ProcessBlock(
          sl,
          dfc.owner.ref,
          dfc.getMeta,
          dfc.tags
        ).addMember
      block.asFE
    def all(using DFC): Block =
      ir.ProcessBlock(
        ir.ProcessBlock.Sensitivity.All,
        dfc.owner.ref,
        dfc.getMeta,
        dfc.tags
      ).addMember
        .asFE
    def initial(using DFC): Block =
      ir.ProcessBlock(
        ir.ProcessBlock.Sensitivity.Initial,
        dfc.owner.ref,
        dfc.getMeta,
        dfc.tags
      ).addMember
        .asFE
  end Block

  object Ops:
    protected type EDDomainOnly[A] = AssertGiven[
      A <:< DomainType.ED,
      "A process with a sensitivity list is only allowed under event-driven (ED) domains."
    ]
    protected type NotDFDomain[A] = AssertGiven[
      util.NotGiven[A <:< DomainType.DF],
      "A process is not supported under dataflow (DF) domains."
    ]
    protected type NoNestingProcess = AssertGiven[
      util.NotGiven[DFC.Scope.Process],
      "A process cannot be nested inside another process."
    ]
    protected type NoNestingInitial = AssertGiven[
      util.NotGiven[DFC.Scope.Initial],
      "A process or an `initial` block cannot be nested inside an `initial` block."
    ]
    protected type InitialNotDFDomain[A] = AssertGiven[
      util.NotGiven[A <:< DomainType.DF],
      "An `initial` block is not supported under dataflow (DF) domains."
    ]
    protected type InitialNotInsideProcess = AssertGiven[
      util.NotGiven[DFC.Scope.Process],
      "An `initial` block cannot be nested inside a process."
    ]
    object process:
      def apply(dfVal: DFValAny, dfVals: DFValAny*)(block: DFC.Scope.Process ?=> Unit)(using
          dt: DomainType
      )(using EDDomainOnly[dt.type], NoNestingProcess, NoNestingInitial, DFC): Unit =
        val owner = Block.list(dfVal :: dfVals.toList)
        dfc.enterOwner(owner)
        block(using DFC.Scope.Process)
        dfc.exitOwner()
      def forever(block: DFC.Scope.Process ?=> Unit)(using
          dt: DomainType
      )(using NotDFDomain[dt.type], NoNestingProcess, NoNestingInitial, DFC): Unit =
        val owner = Block.list(Nil)
        dfc.enterOwner(owner)
        block(using DFC.Scope.Process)
        dfc.exitOwner()
      def apply(all: SameElementsVector.type)(block: DFC.Scope.Process ?=> Unit)(using
          dt: DomainType
      )(using EDDomainOnly[dt.type], NoNestingProcess, NoNestingInitial, DFC): Unit =
        val owner = Block.all
        dfc.enterOwner(owner)
        block(using DFC.Scope.Process)
        dfc.exitOwner()
    end process
    def initial(block: DFC.Scope.Initial ?=> Unit)(using
        dt: DomainType
    )(using InitialNotDFDomain[dt.type], InitialNotInsideProcess, NoNestingInitial, DFC): Unit =
      val owner = Block.initial
      dfc.enterOwner(owner)
      block(using DFC.Scope.Initial)
      dfc.exitOwner()
  end Ops

end Process
