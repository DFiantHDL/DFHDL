package dfhdl.core
import dfhdl.compiler.ir
import dfhdl.internals.*

object DFFor:
  object Block:
    def apply(iter: DFValOf[DFInt32], range: DFRange[?])(using DFC): DFOwnerAny =
      val block = ir.DFLoop.DFForBlock(
        iteratorRef = iter.asIR.asInstanceOf[ir.DFVal.Dcl].refTW[ir.DFLoop.DFForBlock],
        rangeRef = range.asIR.refTW[ir.DFLoop.DFForBlock],
        ownerRef = dfc.owner.ref,
        meta = dfc.getMeta,
        tags = dfc.tags
      )
      block.addMember.asFE
  end Block
  def pluginGetLoopIter[V <: DFValAny](meta: ir.Meta)(using DFC): V =
    dfc.mutableDB.DesignContext.getLoopIter(meta).asInstanceOf[V]
  def plugin(
      iterMeta: ir.Meta,
      forPos: Position,
      range: DFRange[?],
      guards: List[() => DFValOf[DFBool]]
  )(
      run: => Unit
  )(using DFC): Unit =
    val iter = DFVal.Dcl.iterator(using dfc.setMeta(iterMeta))
    dfc.mutableDB.DesignContext.addLoopIter(iterMeta, iter)
    val block = Block(iter, range)(using dfc.setMetaAnon(forPos))
    dfc.enterOwner(block)
    guards.foreach { guard =>
      val guardVal = guard()
      val guardHeader = DFIf.Header(DFUnit)
      val guardBlock = DFIf.Block(Some(guardVal), guardHeader)
      dfc.enterOwner(guardBlock)
    }
    run
    guards.foreach { _ => dfc.exitOwner() }
    dfc.exitOwner()
  end plugin
end DFFor
