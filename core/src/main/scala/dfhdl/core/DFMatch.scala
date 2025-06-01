package dfhdl.core

import dfhdl.internals.*
import dfhdl.compiler.ir
import dfhdl.compiler.printing.*
import ir.DFConditional.{DFCaseBlock, DFMatchHeader}
import DFCaseBlock.Pattern
import dfhdl.compiler.ir.DFConditional
import dfhdl.compiler.ir.DFDecimal.NativeType

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

  def fromCasesExact[R](
      selector: DFValAny,
      cases: List[(Pattern, Option[DFValOf[DFBool]], () => DFVal.TC.Exact[DFTypeAny])],
      forceAnonymous: Boolean
  )(using DFC): R =
    val ifFunc: DFTypeAny => DFValOf[DFTypeAny] = dfType =>
      fromCases(
        selector,
        cases.map { case (pattern, guardOption, run) =>
          (pattern, guardOption, () => run.apply().apply(dfType))
        },
        forceAnonymous
      )
    val exact = new DFVal.TC.Exact[DFTypeAny]:
      type ExactFrom = DFValOf[DFTypeAny]
      type ExactTC = DFVal.TCDummy.type
      val tc: ExactTC = DFVal.TCDummy
      def apply(dfType: DFTypeAny)(using ctx: DFC): tc.Out = ifFunc(dfType).asInstanceOf[tc.Out]
    exact.asInstanceOf[R]
  end fromCasesExact

  def fromCases[R](
      selector: DFValAny,
      cases: List[(Pattern, Option[DFValOf[DFBool]], () => R)],
      forceAnonymous: Boolean
  )(using dfc: DFC): R = try
    import dfc.getSet
    val dfcAnon = summon[DFC].anonymize
    val header =
      Header(DFUnit, selector)(using if (forceAnonymous) dfcAnon else dfc)
    // creating a hook to save the return value for the first branch run
    var firstCaseRet: Option[R] = None
    val firstCaseRun: () => R = () =>
      firstCaseRet = Some(cases.head._3())
      firstCaseRet.get
    var dfTypes = List.empty[ir.DFType]
    var typesAreSimilar = true
    val firstCase =
      singleCase(cases.head._1, cases.head._2, header, firstCaseRun)
    dfTypes = firstCase._1.asIR :: dfTypes
    val (retDFType, _) =
      cases.drop(1).foldLeft(firstCase) { case ((prevDFType, prevBlock), curCase) =>
        val (dfType, block) =
          singleCase(curCase._1, curCase._2, prevBlock, curCase._3)(using dfcAnon)
        dfTypes = dfType.asIR :: dfTypes
        val commonDFType =
          if (dfType.asIR.isSimilarTo(prevDFType.asIR)) prevDFType
          else
            typesAreSimilar = false
            DFUnit
        (commonDFType, block)
      }
    if (typesAreSimilar)
      retDFType match
        case DFUnit => firstCaseRet.get
        case _      =>
          val DFVal(headerIR: DFMatchHeader) = header: @unchecked
          val headerUpdate = headerIR.copy(dfType = retDFType.asIR.dropUnreachableRefs)
          // updating the type of the match header
          headerIR.replaceMemberWith(headerUpdate).asValAny.asInstanceOf[R]
    else
      given printer: Printer = DefaultPrinter(using dfc.getSet)
      val err = DFError.Basic(
        "match",
        new IllegalArgumentException(
          s"""|This DFHDL `match` expression has different return types for cases.
              |These are its branch types in order:
              |${dfTypes.view.reverse.map(t => printer.csDFType(t)).mkString("\n")}
              |""".stripMargin
        )
      )
      dfc.logError(err)
      err.asVal[DFTypeAny, ModifierAny].asInstanceOf[R]
    end if
  catch case e: DFError => DFVal(DFError.Derived(e)).asInstanceOf[R]
  end fromCases

  object Header:
    def apply(dfType: DFTypeAny, selector: DFValAny)(using DFC): DFValAny =
      val header: ir.DFVal = DFMatchHeader(
        dfType.asIR.dropUnreachableRefs,
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
    object to:
      infix def unapply[S <: Boolean, W <: IntP, N <: NativeType](
          arg: DFValOf[DFXInt[S, W, N]]
      ): Option[(Int, Int)] = ???

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
