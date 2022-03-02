package DFiant.core
import DFiant.internals.*
import DFiant.compiler.ir

object Always:
  type Block = DFOwner[ir.AlwaysBlock]
  object Block:
    def list(dfVals: List[DFValAny])(using DFC): Block =
      lazy val sl = ir.AlwaysBlock.Sensitivity.List(dfVals.map(_.asIRForced.refTW(block)))
      lazy val block: ir.AlwaysBlock =
        ir.AlwaysBlock(
          sl,
          dfc.owner.ref,
          dfc.getMeta,
          ir.DFTags.empty
        ).addMember
      block.asFE
    def all(using DFC): Block =
      ir.AlwaysBlock(
        ir.AlwaysBlock.Sensitivity.All,
        dfc.owner.ref,
        dfc.getMeta,
        ir.DFTags.empty
      ).addMember
        .asFE
  end Block

  object Ops:
    object always:
      def apply(dfVals: DFValAny*)(block: => Unit)(using DFC): Unit =
        val owner = Block.list(dfVals.toList)
        dfc.enterOwner(owner)
        block
        dfc.exitOwner()
      def all(block: => Unit)(using DFC): Unit =
        val owner = Block.all
        dfc.enterOwner(owner)
        block
        dfc.exitOwner()

end Always
