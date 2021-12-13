package DFiant.core
import DFiant.internals.*
import DFiant.compiler.ir
import DFiant.compiler.printing.*

final class DFIf[R](
    condOption: Option[DFValOf[DFBool]],
    block: => R,
    prevIfOption: Option[DFIf[R]]
)(using DFC):
  private[DFiant] final val dfc: DFC = summon[DFC]
  protected final val owner: DFOwner =
    DFIf.Block(condOption, prevIfOption.map(_.owner))
  dfc.enterOwner(owner)
  val ret: R = block
  dfc.exitOwner()
  def elseifdf(cond: DFValOf[DFBool], block: => R): DFIf[R] =
    new DFIf[R](Some(cond), block, Some(this))
  def elsedf(block: => R): R =
    val dfif = new DFIf[R](None, block, Some(this))
    dfif.ret
end DFIf

object ifdf:
  def apply[R](cond: DFValOf[DFBool], block: => R)(using DFC): DFIf[R] =
    new DFIf[R](Some(cond), block, None)
//  def fromBranches[R](
//      branches: List[(DFValOf[DFBool], () => R)],
//      elseOption: Option[() => R]
//  )(using DFC): R =
//    val firstIf = DFIf.Block(Some(branches.head._1), None)

object DFIf:
  object Block:
    def apply(
        condOption: Option[DFValOf[DFBool]],
        prevBlockOption: Option[DFOwner]
    )(using
        DFC
    ): DFOwner =
      lazy val condRef: ir.DFVal.Ref = condOption match
        case Some(cond) => cond.asIR.refTW(block)
        case None       => ir.DFRef.TwoWay.Empty
      lazy val prevBlockRef: ir.DFIfElseBlock.Ref = prevBlockOption match
        case Some(prevBlock) =>
          prevBlock.asIR.asInstanceOf[ir.DFIfElseBlock].ref
        case None => ir.DFRef.OneWay.Empty
      lazy val block: ir.DFIfElseBlock =
        ir.DFIfElseBlock(
          condRef,
          prevBlockRef,
          dfc.owner.ref,
          dfc.getMeta,
          ir.DFTags.empty
        ).addMember
      block.asFE
    end apply
  end Block
end DFIf
