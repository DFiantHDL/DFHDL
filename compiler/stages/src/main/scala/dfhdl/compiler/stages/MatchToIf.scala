package dfhdl.compiler.stages

import dfhdl.compiler.analysis.*
import dfhdl.compiler.ir.*
import dfhdl.compiler.patching.*
import dfhdl.options.CompilerOptions
import dfhdl.core.DFIf
import DFConditional.{DFMatchHeader, DFCaseBlock, DFIfHeader, DFIfElseBlock}
import scala.collection.mutable
import dfhdl.core.DFType.asFE
import dfhdl.core.{DFValAny, refTW}
import DFCaseBlock.Pattern
import dfhdl.compiler.stages.verilog.VerilogDialect
import dfhdl.compiler.stages.vhdl.VHDLDialect
import DFVal.Func.Op as FuncOp
import dfhdl.internals.*

/** This stage transforms match statements/expressions to if statements/expressions
  */
case object MatchToIf extends Stage:
  override def dependencies: List[Stage] = List(DropBinds)
  override def nullifies: Set[Stage] = Set(DFHDLUniqueNames, DropUnreferencedAnons)
  def matchFilter(mh: DFMatchHeader)(using getSet: MemberGetSet, co: CompilerOptions): Boolean =
    def composedPatternRemoval = mh.selectorRef.get.dfType match
      case _: ComposedDFType => true
      case _                 => false
    def guardsRemoval = mh.getCBList.exists { mc =>
      mc.guardRef.get != DFMember.Empty
    }
    def wildcardsSupported = co.backend match
      case be: dfhdl.backends.verilog =>
        be.dialect match
          case VerilogDialect.v95 | VerilogDialect.v2001 => false
          case _                                         => true
      case be: dfhdl.backends.vhdl =>
        be.dialect match
          case VHDLDialect.v93 => false
          case _               => true
    def wildcardsRemoval = !wildcardsSupported && mh.hasWildcards
    composedPatternRemoval || guardsRemoval || wildcardsRemoval
  end matchFilter
  def transform(designDB: DB)(using getSet: MemberGetSet, co: CompilerOptions): DB =
    given RefGen = RefGen.fromGetSet
    val patchList: List[(DFMember, Patch)] =
      designDB.members.view
        // only match headers and case blocks that match filter
        .flatMap {
          case mh: DFMatchHeader if matchFilter(mh) =>
            val ih = DFIfHeader(mh.dfType, mh.ownerRef, mh.meta, mh.tags)
            val selectorIR = mh.selectorRef.get
            // anonymous selectors are named as `match_sel`
            val updatedSelectorIR =
              if (selectorIR.isAnonymous) selectorIR.setName("match_sel") else selectorIR
            val nameAnonSelPatchOpt =
              if (selectorIR.isAnonymous)
                Some(
                  selectorIR -> Patch.Replace(
                    updatedSelectorIR,
                    Patch.Replace.Config.FullReplacement
                  )
                )
              else None
            // this design holds all the created conditions of the patterns
            val condsDsn = new MetaDesign(
              mh,
              Patch.Add.Config.ReplaceWithLast(Patch.Replace.Config.FullReplacement)
            ):
              val selector = updatedSelectorIR.asValAny
              val ifBlockPatches: List[(DFMember, Patch)] = mh.getCBList.map { c =>
                // transform a match pattern to a boolean condition value
                def getPatternCondOpt(
                    // selector by reference to allow lazy struct field selector construction
                    selector: => DFValAny,
                    pattern: Pattern
                ): Option[Boolean <> VAL] =
                  def condListReduce(
                      list: List[Boolean <> VAL],
                      reductionOp: FuncOp
                  ): Option[Boolean <> VAL] =
                    list match
                      case cond :: Nil => Some(cond)
                      case Nil         => None
                      case _ =>
                        Some(dfhdl.core.DFVal.Func(dfhdl.core.DFBool, reductionOp, list))
                  pattern match
                    case Pattern.Singleton(DFRef(const: DFVal.Const)) =>
                      const.dfType match
                        // need to split wildcard bit match pattern
                        case DFBits(_) if const.isBubble =>
                          val bubbleBits = const.data.asInstanceOf[(BitVector, BitVector)]._2
                          val normalRanges = bubbleBits.getFalseRanges
                          val bitsSelector = selector.asValOf[dfhdl.core.DFBits[Int]]
                          val bitsWildcard = const.asValOf[dfhdl.core.DFBits[Int]]
                          val condList = normalRanges.map(r =>
                            bitsSelector.bits(r._1, r._2) == bitsWildcard.bits(r._1, r._2)
                          )
                          condListReduce(condList, FuncOp.&)
                        // regular singleton equality pattern
                        case _ => Some(selector == const.asValAny)
                    case Pattern.Alternative(list) =>
                      val condList = list.flatMap(getPatternCondOpt(selector, _))
                      condListReduce(condList, FuncOp.|)
                    case Pattern.Struct(name, fieldPatterns) =>
                      val fieldMap = selector.dfType.asIR.asInstanceOf[DFStruct].fieldMap
                      val condList = fieldMap.lazyZip(fieldPatterns).flatMap {
                        case ((fieldName, _), fieldPattern) =>
                          // the selector is lazy and will only be constructed if utilized by a pattern
                          lazy val fieldSelector =
                            dfhdl.core.DFVal.Alias.SelectField(selector, fieldName)
                          getPatternCondOpt(fieldSelector, fieldPattern)
                      }.toList
                      condListReduce(condList, FuncOp.&)
                    case Pattern.NamedArg(name, pattern) =>
                      getPatternCondOpt(selector, pattern)
                    case _ => None
                  end match
                end getPatternCondOpt
                val patternCondOpt = getPatternCondOpt(selector, c.pattern)
                val guardRef: DFConditional.Block.GuardRef =
                  (c.getGuardOption, patternCondOpt) match
                    case (_, None)          => c.guardRef
                    case (None, Some(cond)) => cond.asIR.refTW[DFIfElseBlock]
                    case (Some(guardVal), Some(cond)) =>
                      val combinedGuard = guardVal.asValOf[dfhdl.core.DFBool] && cond
                      combinedGuard.asIR.refTW[DFIfElseBlock]
                val ifBlock = DFIfElseBlock(
                  guardRef,
                  c.prevBlockOrHeaderRef.asInstanceOf[DFIfElseBlock.Ref],
                  c.ownerRef,
                  c.meta,
                  c.tags
                )
                val refFilter = new Patch.Replace.RefFilter:
                  def apply(refs: Set[DFRefAny])(using MemberGetSet): Set[DFRefAny] =
                    refs - c.prevBlockOrHeaderRef - c.ownerRef - guardRef
                c -> Patch.Replace(ifBlock, Patch.Replace.Config.FullReplacement)
              }
              plantMember(ih)
            condsDsn.patch :: condsDsn.ifBlockPatches ++ nameAnonSelPatchOpt
          case _ => Nil
        }
        .toList
    designDB.patch(patchList)
  end transform
end MatchToIf

extension [T: HasDB](t: T)
  def matchToIf(using co: CompilerOptions): DB =
    StageRunner.run(MatchToIf)(t.db)
