package DFiant.core

import DFiant.internals.*
import DFiant.compiler.ir
import DFiant.compiler.printing.*

import ir.DFConditional.{DFMatchHeader, DFCaseBlock}
import DFCaseBlock.Pattern

object DFMatch:
  def singleCase[R](
      pattern: Pattern,
      guardOption: Option[DFValOf[DFBool]],
      prevBlockOrHeader: DFOwner | DFValAny,
      run: () => R
  )(using
      DFC
  ): (DFTypeAny, DFOwner) =
    // first we create the header without a known type.
    val block = Block(pattern, guardOption, prevBlockOrHeader)
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
  end singleCase

  def fromCases[R](
      selector: DFValAny,
      cases: List[(Pattern, Option[DFValOf[DFBool]], () => R)]
  )(using DFC): R =
    val header = Header(NoType, selector)
    val dfcAnon = summon[DFC].anonymize
    // creating a hook to save the return value for the first branch run
    var firstCaseRet: Option[R] = None
    val firstCaseRun: () => R = () =>
      firstCaseRet = Some(cases.head._3())
      firstCaseRet.get
    val firstCase =
      singleCase(cases.head._1, cases.head._2, header, firstCaseRun)
    val (retDFType, _) =
      cases.drop(1).foldLeft(firstCase) {
        case ((prevDFType, prevBlock), curCase) =>
          val (dfType, block) =
            singleCase(curCase._1, curCase._2, prevBlock, curCase._3)(using
              dfcAnon
            )
          val commonDFType =
            if (dfType.asIR == prevDFType.asIR) prevDFType else NoType
          (commonDFType, block)
      }
    retDFType match
      case NoType => firstCaseRet.get
      case _ =>
        val DFVal(headerIR: DFMatchHeader) = header
        val headerUpdate = headerIR.copy(dfType = retDFType.asIR)
        // updating the type of the if header
        headerIR.replaceMemberWith(headerUpdate).asValAny.asInstanceOf[R]
  end fromCases

  object Header:
    def apply(dfType: DFTypeAny, selector: DFValAny)(using DFC): DFValAny =
      lazy val header: ir.DFVal = DFMatchHeader(
        dfType.asIR,
        selector.asIR.refTW(header),
        dfc.owner.ref,
        dfc.getMeta,
        ir.DFTags.empty
      ).addMember
      header.asValAny
  end Header

  object Pattern:
    export DFCaseBlock.Pattern.CatchAll
    object Singleton:
      def apply(token: DFTokenAny): DFCaseBlock.Pattern =
        DFCaseBlock.Pattern.Singleton(token.asIR)
  object Block:
    def apply(
        pattern: Pattern,
        guardOption: Option[DFValOf[DFBool]],
        prevBlockOrHeader: DFOwner | DFValAny
    )(using
        DFC
    ): DFOwner =
      lazy val guardRef: ir.DFVal.Ref = guardOption match
        case Some(cond) => cond.asIR.refTW(block)
        case None       => ir.DFRef.TwoWay.Empty
      lazy val prevBlockOrHeaderRef: DFCaseBlock.Ref = prevBlockOrHeader match
        case prevBlock: DFOwner =>
          prevBlock.asIR.asInstanceOf[DFCaseBlock].ref
        case header: DFValAny =>
          header.asIR.asInstanceOf[DFMatchHeader].ref
      lazy val block: DFCaseBlock =
        DFCaseBlock(
          pattern,
          guardRef,
          prevBlockOrHeaderRef,
          dfc.owner.ref,
          dfc.getMeta,
          ir.DFTags.empty
        ).addMember
      block.asFE
    end apply
  end Block
end DFMatch