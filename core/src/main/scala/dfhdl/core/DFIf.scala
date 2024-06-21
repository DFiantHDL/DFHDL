package dfhdl.core
import dfhdl.internals.*
import dfhdl.compiler.ir
import dfhdl.compiler.printing.*

import ir.DFConditional
import ir.DFConditional.{DFIfHeader, DFIfElseBlock}

protected[core] def analyzeControlRet(ret: Any)(using DFC): DFTypeAny = Exact.strip(ret) match
  case v: DFValAny =>
    // adding ident placement as the last member in the control block
    DFVal.Alias.AsIs.ident(v)(using dfc.anonymize)
    v.dfType
  case _ =>
    DFUnit

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
    val dfType = analyzeControlRet(ret)
    dfc.exitOwner()
    (dfType, block)
  end singleBranch
  def fromBranches[R](
      branches: List[(DFValOf[DFBool], () => R)],
      elseOption: Option[() => R]
  )(using DFC): R = try
    val header = Header(DFUnit)
    val dfcAnon = summon[DFC].anonymize
    var branchTypes = List.empty[ir.DFType]
    // creating a hook to save the return value for the first branch run
    var firstIfRet: Option[R] = None
    val firstIfRun: () => R = () =>
      firstIfRet = Some(branches.head._2())
      firstIfRet.get
    val firstIf = singleBranch(Some(branches.head._1), header, firstIfRun)
    branchTypes = firstIf._1.asIR :: branchTypes
    val midIfsBlock =
      branches.drop(1).foldLeft(firstIf._2) { case (prevBlock, branch) =>
        val (dfType, block) =
          singleBranch(Some(branch._1), prevBlock, branch._2)(using dfcAnon)
        branchTypes = dfType.asIR :: branchTypes
        block
      }
    elseOption.foreach { e =>
      val (dfType, _) = singleBranch(None, midIfsBlock, e)(using dfcAnon)
      branchTypes = dfType.asIR :: branchTypes
    }
    val hasNoType = branchTypes.contains(ir.DFUnit)
    // if one branch has DFUnit, the return type is DFUnit.
    // otherwise, all types must be the same.
    if (hasNoType || branchTypes.allElementsAreEqual)
      val retDFType = if (hasNoType) ir.DFUnit else branchTypes.head
      val DFVal(headerIR: DFIfHeader) = header: @unchecked
      val headerUpdate = headerIR.copy(dfType = retDFType)
      // updating the type of the if header
      headerIR.replaceMemberWith(headerUpdate).asValAny.asInstanceOf[R]
    else // violation
      given printer: Printer = DefaultPrinter(using dfc.getSet)
      val err = DFError.Basic(
        "if",
        new IllegalArgumentException(
          s"""|This DFHDL `if` expression has different return types for branches.
              |These are its branch types in order:
              |${branchTypes.view.reverse.map(t => printer.csDFType(t)).mkString("\n")}
              |""".stripMargin
        )
      )
      dfc.logError(err)
      err.asVal[DFTypeAny, ModifierAny].asInstanceOf[R]
    end if
  catch case e: DFError => DFVal(DFError.Derived(e)).asInstanceOf[R]
  end fromBranches

  object Header:
    def apply(dfType: DFTypeAny)(using DFC): DFValAny =
      DFIfHeader(
        dfType.asIR,
        dfc.owner.ref,
        dfc.getMeta,
        dfc.tags
      ).addMember.asValAny
  end Header

  object Block:
    def apply(
        guardOption: Option[DFValOf[DFBool]],
        prevBlockOrHeader: DFOwnerAny | DFValAny
    )(using
        DFC
    ): DFOwnerAny =
      val guardRef: DFConditional.Block.GuardRef = guardOption match
        case Some(cond) => cond.asIR.refTW[DFIfElseBlock]
        case None       => ir.DFRef.TwoWay.Empty
      val prevBlockOrHeaderRef: DFIfElseBlock.Ref = prevBlockOrHeader match
        case prevBlock: DFOwnerAny =>
          prevBlock.asIR.asInstanceOf[DFIfElseBlock].refTW[DFIfElseBlock]
        case header: DFValAny =>
          header.asIR.asInstanceOf[DFIfHeader].refTW[DFIfElseBlock]
      val block: DFIfElseBlock =
        DFIfElseBlock(
          guardRef, prevBlockOrHeaderRef, dfc.owner.ref, dfc.getMeta, dfc.tags
        ).addMember
      block.asFE
    end apply
  end Block
end DFIf
