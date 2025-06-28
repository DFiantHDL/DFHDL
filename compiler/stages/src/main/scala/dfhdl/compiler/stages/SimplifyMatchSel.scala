package dfhdl.compiler.stages

import dfhdl.compiler.analysis.*
import dfhdl.compiler.ir.*
import dfhdl.compiler.patching.*
import dfhdl.options.CompilerOptions
import dfhdl.compiler.stages.vhdl.VHDLDialect
import DFConditional.{DFMatchHeader, DFCaseBlock}
import DFVal.Func.Op as FuncOp

/** This stage simplifies match patterns in VHDL'93:
  *   - drop UInt/SInt match patterns by transforming them to Bits match patterns.
  *   - drop non locally static pattern selection by applying non-parametrized range selection.
  */
case object SimplifyMatchSel extends Stage:
  override def runCondition(using co: CompilerOptions): Boolean =
    co.backend match
      case be: dfhdl.backends.vhdl =>
        be.dialect match
          case VHDLDialect.v93 => true
          case _               => false
      case _ => false
  override def dependencies: List[Stage] = List(MatchToIf)
  override def nullifies: Set[Stage] = Set(ExplicitNamedVars, DFHDLUniqueNames)
  def transform(designDB: DB)(using getSet: MemberGetSet, co: CompilerOptions): DB =
    given RefGen = RefGen.fromGetSet
    // vhdl parameter is locally static for basic operations between locally static values (not design parameter)
    object LocallyStaticVHDL:
      def unapply(dfVal: DFVal): Boolean =
        dfVal match
          case _: DFVal.DesignParam => false
          case func: DFVal.Func     =>
            func.dfType match
              case DFBool | DFBit | DFBits(_) | DFUInt(_) | DFSInt(_) | DFInt32 =>
                func.op match
                  case FuncOp.+ | FuncOp.- | FuncOp.`*` | FuncOp./ | FuncOp.% | FuncOp.** | FuncOp.=== | FuncOp.=!= | FuncOp.< | FuncOp.<= | FuncOp.> | FuncOp.>= =>
                    func.args.forall(r => LocallyStaticVHDL.unapply(r.get))
                  case _ => false
              case _ => false
          case _ => true
    object SimplifySelector:
      def unapply(selector: DFVal): Boolean =
        selector.dfType match
          // UInt/SInt selectors must be simplified
          case DFUInt(_) | DFSInt(_) => true
          // Bits selectors must be simplified if the referenced width is not locally static
          case DFBits(DFRef(dfVal: DFVal)) => !LocallyStaticVHDL.unapply(dfVal)
          case _                           => false
    end SimplifySelector
    val patchList: List[(DFMember, Patch)] =
      designDB.members.view
        .flatMap {
          // handling match selector
          case mh @ DFMatchHeader(selectorRef = DFRef(selector @ SimplifySelector())) =>
            val dsn = new MetaDesign(mh, Patch.Add.Config.Before):
              // named according to the original selector, unless it is anonymous then using the name suggestion mechanism
              val newSelectorName =
                val name =
                  if (selector.isAnonymous) selector.suggestName.getOrElse("anon_sel")
                  else selector.getName
                name + "_slv"
              val namedDFC = dfc.setName(newSelectorName)
              // indicates if selector is design-parameterized
              val isLocallyStatic = selector.dfType.getRefs.forall {
                case DFRef(LocallyStaticVHDL()) => true
                case _                          => false
              }
              // the selector as Bits
              val convertedSelector = selector.dfType match
                case DFBits(_) => selector
                case _         =>
                  // if the selector is not locally static, then don't name the converted selector,
                  // and leave it anonymous. The name will be used for the range selection.
                  val convertDFC = if (isLocallyStatic) namedDFC else dfc
                  selector.asValAny.bits(using convertDFC).asIR
              // the final selector after optional conversion and optional range selection
              val updatedSelector =
                if (isLocallyStatic) convertedSelector
                else convertedSelector.asValOf[dfhdl.core.DFBits[Int]].apply(
                  selector.dfType.width - 1,
                  0
                )(using namedDFC).asIR
            // replace only for the match header
            val refFilter = new Patch.Replace.RefFilter:
              def apply(refs: Set[DFRefAny])(using MemberGetSet): Set[DFRefAny] =
                Set(mh.selectorRef)
            val selectorPatch =
              selector -> Patch.Replace(
                dsn.updatedSelector,
                Patch.Replace.Config.ChangeRefOnly,
                refFilter
              )
            List(dsn.patch, selectorPatch)
          // handling case block patterns
          case cb: DFCaseBlock =>
            cb.getRefs.view.filterNot(_.isTypeRef).flatMap {
              case DFRef(const @ DFVal.Const(dfType = DFUInt(_) | DFSInt(_))) =>
                val newConst =
                  const.copy(
                    dfType = DFBits(const.dfType.width),
                    data = const.dfType.dataToBitsData(const.data.asInstanceOf[const.dfType.Data])
                  )
                Some(const -> Patch.Replace(newConst, Patch.Replace.Config.FullReplacement))
              case _ => None
            }
          case _ => None
        }
        .toList
    designDB.patch(patchList)
  end transform
end SimplifyMatchSel

extension [T: HasDB](t: T)
  def simplifyMatchSel(using co: CompilerOptions): DB =
    StageRunner.run(SimplifyMatchSel)(t.db)
