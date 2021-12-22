package DFiant.core
import DFiant.internals.*
import DFiant.compiler.ir
import DFiant.compiler.printing.*

object DFIf:
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
