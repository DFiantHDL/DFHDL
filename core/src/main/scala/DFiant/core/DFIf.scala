package DFiant.core
import DFiant.internals.*
import DFiant.compiler.ir
import DFiant.compiler.printing.*

import ir.DFConditional.{DFIfHeader, DFIfElseBlock}
object DFIf:
  def singleBranch[R](
      condOption: Option[DFValOf[DFBool]],
      prevBlockOrHeader: DFOwnerAny | DFValAny,
      run: () => R
  )(using
      DFC
  ): (DFTypeAny, DFOwnerAny) =
    // first we create the header without a known type.
    val block = DFIf.Block(condOption, prevBlockOrHeader)
    dfc.enterOwner(block)
    // now all members of the branch will be constructed
    val ret: R = run()
    val dfType = ret match
      case v: DFValAny =>
        // adding ident placement as the last member in the if
        DFVal.Alias.AsIs.ident(v)(using dfc.anonymize)
        v.dfType
      case _ =>
        NoType
    dfc.exitOwner()
    (dfType, block)
  end singleBranch
  def fromBranches[R](
      branches: List[(DFValOf[DFBool], () => R)],
      elseOption: Option[() => R]
  )(using DFC): R =
    val header = Header(NoType)
    val dfcAnon = summon[DFC].anonymize
    // creating a hook to save the return value for the first branch run
    var firstIfRet: Option[R] = None
    val firstIfRun: () => R = () =>
      firstIfRet = Some(branches.head._2())
      firstIfRet.get
    val firstIf = singleBranch(Some(branches.head._1), header, firstIfRun)
    val midIfs =
      branches.drop(1).foldLeft(firstIf) { case ((prevDFType, prevBlock), branch) =>
        val (dfType, block) =
          singleBranch(Some(branch._1), prevBlock, branch._2)(using dfcAnon)
        val commonDFType =
          if (dfType.asIR == prevDFType.asIR) prevDFType else NoType
        (commonDFType, block)
      }
    val retDFType = elseOption
      .map { e =>
        val (dfType, _) = singleBranch(None, midIfs._2, e)(using dfcAnon)
        if (dfType.asIR == midIfs._1.asIR) midIfs._1 else NoType
      }
      .getOrElse(midIfs._1)
    retDFType match
      case NoType => firstIfRet.get
      case _ =>
        val DFVal(headerIR: DFIfHeader) = header
        val headerUpdate = headerIR.copy(dfType = retDFType.asIR)
        // updating the type of the if header
        headerIR.replaceMemberWith(headerUpdate).asValAny.asInstanceOf[R]
  end fromBranches

  object Header:
    def apply(dfType: DFTypeAny)(using DFC): DFValAny =
      DFIfHeader(
        dfType.asIR,
        dfc.owner.ref,
        dfc.getMeta,
        ir.DFTags.empty
      ).addMember.asValAny
  end Header

  object Block:
    def apply(
        condOption: Option[DFValOf[DFBool]],
        prevBlockOrHeader: DFOwnerAny | DFValAny
    )(using
        DFC
    ): DFOwnerAny =
      lazy val condRef: DFIfElseBlock.CondRef = condOption match
        case Some(cond) => cond.asIR.refTW(block)
        case None       => ir.DFRef.TwoWay.Empty
      lazy val prevBlockOrHeaderRef: DFIfElseBlock.Ref = prevBlockOrHeader match
        case prevBlock: DFOwnerAny =>
          prevBlock.asIR.asInstanceOf[DFIfElseBlock].ref
        case header: DFValAny =>
          header.asIR.asInstanceOf[DFIfHeader].ref
      lazy val block: DFIfElseBlock =
        DFIfElseBlock(
          condRef,
          prevBlockOrHeaderRef,
          dfc.owner.ref,
          dfc.getMeta,
          ir.DFTags.empty
        ).addMember
      block.asFE
    end apply
  end Block
end DFIf
