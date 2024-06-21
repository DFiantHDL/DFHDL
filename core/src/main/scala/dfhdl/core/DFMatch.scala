package dfhdl.core

import dfhdl.internals.*
import dfhdl.compiler.ir
import dfhdl.compiler.printing.*
import ir.DFConditional.{DFCaseBlock, DFMatchHeader}
import DFCaseBlock.Pattern
import dfhdl.compiler.ir.DFConditional

object DFMatch:
  def singleCase[R](
      pattern: Pattern,
      guardOption: Option[DFValOf[DFBool]],
      prevBlockOrHeader: DFOwnerAny | DFValAny,
      run: () => R
  )(using
      DFC
  ): (DFTypeAny, DFOwnerAny) =
    // first we create the header without a known type.
    val block = Block(pattern, guardOption, prevBlockOrHeader)
    dfc.enterOwner(block)
    // now all members of the branch will be constructed
    val ret: R = run()
    val dfType = analyzeControlRet(ret)
    dfc.exitOwner()
    (dfType, block)
  end singleCase

  def fromCases[R](
      selector: DFValAny,
      cases: List[(Pattern, Option[DFValOf[DFBool]], () => R)],
      forceAnonymous: Boolean
  )(using DFC): R = try
    val dfcAnon = summon[DFC].anonymize
    val header =
      Header(DFUnit, selector)(using if (forceAnonymous) dfcAnon else dfc)
    // creating a hook to save the return value for the first branch run
    var firstCaseRet: Option[R] = None
    val firstCaseRun: () => R = () =>
      firstCaseRet = Some(cases.head._3())
      firstCaseRet.get
    val firstCase =
      singleCase(cases.head._1, cases.head._2, header, firstCaseRun)
    val (retDFType, _) =
      cases.drop(1).foldLeft(firstCase) { case ((prevDFType, prevBlock), curCase) =>
        val (dfType, block) =
          singleCase(curCase._1, curCase._2, prevBlock, curCase._3)(using dfcAnon)
        val commonDFType =
          if (dfType.asIR == prevDFType.asIR) prevDFType else DFUnit
        (commonDFType, block)
      }
    retDFType match
      case DFUnit => firstCaseRet.get
      case _ =>
        val DFVal(headerIR: DFMatchHeader) = header: @unchecked
        val headerUpdate = headerIR.copy(dfType = retDFType.asIR)
        // updating the type of the if header
        headerIR.replaceMemberWith(headerUpdate).asValAny.asInstanceOf[R]
  catch case e: DFError => DFVal(DFError.Derived(e)).asInstanceOf[R]
  end fromCases

  object Header:
    def apply(dfType: DFTypeAny, selector: DFValAny)(using DFC): DFValAny =
      val header: ir.DFVal = DFMatchHeader(
        dfType.asIR,
        selector.asIR.refTW[ir.DFVal],
        dfc.owner.ref,
        dfc.getMeta,
        dfc.tags
      ).addMember
      header.asValAny
  end Header

  object Pattern:
    export DFCaseBlock.Pattern.CatchAll
    object Singleton:
      def apply(value: DFValAny)(using DFC): DFCaseBlock.Pattern =
        DFCaseBlock.Pattern.Singleton(value.asIR.refTW[DFCaseBlock])
  object Block:
    def apply(
        pattern: Pattern,
        guardOption: Option[DFValOf[DFBool]],
        prevBlockOrHeader: DFOwnerAny | DFValAny
    )(using
        DFC
    ): DFOwnerAny =
      val guardRef: DFConditional.Block.GuardRef = guardOption match
        case Some(cond) => cond.asIR.refTW[DFCaseBlock]
        case None       => ir.DFRef.TwoWay.Empty
      val prevBlockOrHeaderRef: DFCaseBlock.Ref = prevBlockOrHeader match
        case prevBlock: DFOwnerAny =>
          prevBlock.asIR.asInstanceOf[DFCaseBlock].refTW[DFCaseBlock]
        case header: DFValAny =>
          header.asIR.asInstanceOf[DFMatchHeader].refTW[DFCaseBlock]
      val block: DFCaseBlock =
        DFCaseBlock(
          pattern, guardRef, prevBlockOrHeaderRef, dfc.owner.ref, dfc.getMeta, dfc.tags
        ).addMember
      block.asFE
    end apply
  end Block
end DFMatch
