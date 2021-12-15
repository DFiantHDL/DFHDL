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
  def singleBranch[R](
      condOption: Option[DFValOf[DFBool]],
      prevBlockOption: Option[DFOwner],
      run: () => R
  )(using
      DFC
  ): DFOwner =
    // first we create the header without a known type.
    val ifHeaderBlock = DFIf.Block(NoType, condOption, prevBlockOption)
    dfc.enterOwner(ifHeaderBlock)
    // now all members of the branch will be constructed
    val ifRet: R = run()
    val fixedIfHeader = ifRet match
      case v: DFValAny =>
        // adding ident placement as the last member in the if
        DFVal.Alias.AsIs.ident(v)(using dfc.anonymize)
        val ifHeaderBlockIR = ifHeaderBlock.asIR.asInstanceOf[ir.DFIfElseBlock]
        val ifHeaderBlockUpdate = ifHeaderBlockIR.copy(dfType = v.dfType.asIR)
        // updating the type of the if header
        ifHeaderBlockIR.replaceMemberWith(ifHeaderBlockUpdate).asFE
      case _ =>
        ifHeaderBlock
    dfc.exitOwner()
    fixedIfHeader
  end singleBranch
  def fromBranches[R](
      branches: List[(DFValOf[DFBool], () => R)],
      elseOption: Option[() => R]
  )(using DFC): R =
    val dfcAnon = summon[DFC].anonymize
    // creating a hook to save the return value for the first branch run
    var firstIfRet: Option[R] = None
    val firstIfRun: () => R = () =>
      firstIfRet = Some(branches.head._2())
      firstIfRet.get
    val firstIf = singleBranch(Some(branches.head._1), None, firstIfRun)
    val midIfs =
      branches.drop(1).foldLeft(List(firstIf)) { case (prevIfs, branch) =>
        singleBranch(Some(branch._1), Some(prevIfs.head), branch._2)(using
          dfcAnon
        ) :: prevIfs
      }
    val allIfs = elseOption
      .map { e =>
        singleBranch(None, Some(midIfs.head), e)(using dfcAnon) :: midIfs
      }
      .getOrElse(midIfs)

    val allTypes: List[ir.DFType] = allIfs.view
      .map(_.asIR)
      .map {
        case r: ir.DFVal => r.dfType
        case _ =>
          return firstIfRet.get
      }
      .toList
    val sameType = allTypes.forall(_ == allTypes.head)
    if (sameType)
      new DFVal(firstIf.asIR.asInstanceOf[ir.DFVal]).asInstanceOf[R]
    else firstIfRet.get
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
