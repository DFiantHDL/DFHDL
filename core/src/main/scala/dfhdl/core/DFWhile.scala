package dfhdl.core
import dfhdl.compiler.ir
import dfhdl.internals.*
import scala.annotation.implicitNotFound

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

protected[dfhdl] object LoopOps:
  // to be wrapped around a block of code to indicate that the loop is combinational
  @metaContextForward(0)
  def COMB_LOOP[T](block: (DFC, DFRange.HasDFRange) ?=> T)(using
      dfc: DFC,
      @implicitNotFound(
        "`COMB_LOOP` is only allowed under register-transfer (RT) domains."
      ) rt: DomainType.RT
  ): T =
    val dfcWithCombTag = dfc.tag(ir.CombinationalTag)
    block(using dfcWithCombTag, new DFRange.HasDFRange {})

  // to be wrapped around a block of code to indicate that the loop should fall through
  // to the next step if the guard is false without consuming any cycles
  @metaContextForward(0)
  def FALL_THROUGH[T](block: DFC ?=> T)(using
      dfc: DFC,
      @implicitNotFound(
        "`FALL_THROUGH` is only allowed under register-transfer (RT) domains."
      ) rt: DomainType.RT
  ): T =
    val dfcWithCombTag = dfc.tag(ir.FallThroughTag)
    block(using dfcWithCombTag)
end LoopOps
