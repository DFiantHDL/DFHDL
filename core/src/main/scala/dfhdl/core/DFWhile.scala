package dfhdl.core
import dfhdl.compiler.ir
import dfhdl.internals.*
import scala.annotation.implicitNotFound

object DFWhile:
  object Block:
    def apply(guard: DFValOf[DFBoolOrBit], fallThrough: Boolean = false)(using DFC): DFOwnerAny =
      val block = ir.DFLoop.DFWhileBlock(
        guardRef = guard.asIR.refTW[ir.DFLoop.DFWhileBlock],
        ownerRef = dfc.owner.ref,
        meta = dfc.getMeta,
        tags = if (fallThrough) dfc.tags.tag(ir.FallThroughTag) else dfc.tags
      )
      block.addMember.asFE
  end Block
  def plugin(guard: DFValOf[DFBoolOrBit], fallThrough: Boolean = false)(run: => Unit)(using
      DFC
  ): Unit =
    val block = Block(guard, fallThrough)
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

  // Marks a single loop as falling through: when its condition is false on entry, the loop is
  // skipped without consuming a cycle, continuing straight to the next step. Unlike `COMB_LOOP`,
  // which marks a whole region (every loop under a combinational loop must itself be
  // combinational), this is a property of one loop, so it is written on that loop's own condition
  // or range and a nested loop must opt in again.
  //
  // Both forms are replaced by the compiler plugin, which passes the mark to the loop being
  // constructed and rejects the call anywhere other than a `while` condition or a `for` range.
  private inline def pluginReplaced: Nothing =
    throw new IllegalArgumentException(
      "FALL_THROUGH is not meant to be run directly, the DFHDL compiler plugin should have replaced its call."
    )
  @metaContextForward(0)
  def FALL_THROUGH(cond: DFValOf[DFBoolOrBit])(using
      dfc: DFC,
      @implicitNotFound(
        "`FALL_THROUGH` is only allowed under register-transfer (RT) domains."
      ) rt: DomainType.RT
  ): DFValOf[DFBoolOrBit] = pluginReplaced
  @metaContextForward(0)
  def FALL_THROUGH[P](range: DFRange[P])(using
      dfc: DFC,
      @implicitNotFound(
        "`FALL_THROUGH` is only allowed under register-transfer (RT) domains."
      ) rt: DomainType.RT
  ): DFRange[P] = pluginReplaced
end LoopOps
