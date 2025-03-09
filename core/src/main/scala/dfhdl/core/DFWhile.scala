package dfhdl.core
import dfhdl.compiler.ir
import dfhdl.internals.*

object DFWhile:
  object Block:
    def apply(guard: DFValOf[DFBoolOrBit])(using DFC): DFOwnerAny =
      val block = ir.DFLoop.DFWhileBlock(
        guardRef = guard.asIR.refTW[ir.DFLoop.DFWhileBlock],
        ownerRef = dfc.owner.ref,
        meta = dfc.getMeta,
        tags = dfc.tags
      )
      block.addMember.asFE
  end Block
  def plugin(guard: DFValOf[DFBoolOrBit])(run: => Unit)(using DFC): Unit =
    val block = Block(guard)
    dfc.enterOwner(block)
    run
    dfc.exitOwner()
end DFWhile
