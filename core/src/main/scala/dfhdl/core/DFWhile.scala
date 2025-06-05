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

//to be used inside an RT loop to indicate that the loop is combinational
def COMB_LOOP(using
    dfc: DFC,
    @implicitNotFound(
      "`COMB_LOOP` is only allowed under register-transfer (RT) domains."
    ) rt: DomainType.RT
): Unit =
  import dfc.getSet
  var ownerIR = dfc.owner.asIR
  var stop = false
  var lineEnd = -1
  while (!stop)
    ownerIR match
      case cb: ir.DFConditional.Block => ownerIR = cb.getOwner
      case lb: ir.DFLoop.Block =>
        if (lineEnd == -1)
          lineEnd = lb.meta.position.lineEnd
        else if (lineEnd != lb.meta.position.lineEnd)
          stop = true
        if (!stop)
          ownerIR.setTags(_.tag(ir.CombinationalTag))
          ownerIR = lb.getOwner
      case _ => stop = true
end COMB_LOOP
