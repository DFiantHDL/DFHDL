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
    DFIf.Block(NoType, condOption, prevIfOption.map(_.owner))
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
  def fromBranches[R](
      branches: List[(DFValOf[DFBool], () => R)],
      elseOption: Option[() => R]
  )(using DFC): R =
    val dfcAnon = summon[DFC].anonymize
    val firstIf = DFIf.Block(NoType, Some(branches.head._1), None)
    dfcAnon.enterOwner(firstIf)
    val firstIfRet: R = branches.head._2()
    dfcAnon.exitOwner()
    val (elseIfs, elseIfsRets) =
      branches.drop(1).foldLeft((firstIf, List(firstIfRet))) {
        case ((prevIf, prevIfRet), branch) =>
          val lastIf =
            DFIf.Block(NoType, Some(branch._1), Some(prevIf))(using dfcAnon)
          dfcAnon.enterOwner(lastIf)
          val lastIfRet: R = branch._2()
          dfcAnon.exitOwner()
          (lastIf, lastIfRet :: prevIfRet)
      }
    val elseRet = elseOption.map { e =>
      val lastIf = DFIf.Block(NoType, None, Some(elseIfs))(using dfcAnon)
      dfcAnon.enterOwner(lastIf)
      val lastIfRet: R = e()
      dfcAnon.exitOwner()
      lastIfRet
    }
    val allRets: List[R] = elseIfsRets ++ elseRet
    val allTypes: List[DFTypeAny] = allRets
      .map {
        case r: DFValAny => r.dfType
        case _ =>
          return firstIfRet
      }
    val sameType = allTypes.forall(_ == allTypes.head)
    if (sameType)
      val firstIfIR = firstIf.asIR.asInstanceOf[ir.DFIfElseBlock]
      val firstIfIRUpdate = firstIfIR.copy(dfType = allTypes.head.asIR)
      firstIfIR.replaceMemberWith(firstIfIRUpdate)
      new DFVal(firstIfIRUpdate).asInstanceOf[R]
    else firstIfRet
  end fromBranches
end ifdf

object DFIf:
  object Block:
    def apply(
        dfType: DFTypeAny,
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
          dfType.asIR,
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
