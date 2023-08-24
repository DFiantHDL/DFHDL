package dfhdl.plugin

import dotty.tools.dotc.*
import plugins.*
import core.*
import Contexts.*
import Symbols.*
import Flags.*
import SymDenotations.*
import Decorators.*
import ast.Trees.*
import ast.{tpd, untpd}
import StdNames.nme
import Names.*
import Constants.Constant
import Types.*
import dotty.tools.dotc.semanticdb.ConstantMessage.SealedValue.{
  BooleanConstant,
  IntConstant,
  UnitConstant
}

import scala.language.implicitConversions
import collection.{mutable, immutable}
import annotation.tailrec
import scala.util.boundary, boundary.break

extension (value: BigInt)
  def bitsWidth(signed: Boolean): Int =
    if (value > 0)
      if (signed) value.bitLength + 1 else value.bitLength
    else if (value == 0)
      if (signed) 2 else 1
    else if (value == -1) 2
    else value.bitLength + 1 // value < 0
extension (value: Int) def bitsWidth(signed: Boolean): Int = BigInt(value).bitsWidth(signed)

class CustomControlPhase(setting: Setting) extends CommonPhase:
  import tpd._

  val phaseName = "CustomIf"
//  override val debugFilter: String => Boolean =
//    _.contains("DFMatchSpec.scala")
  override val runsAfter = Set(transform.Pickler.name)
  override val runsBefore = Set("MetaContextGen")
  val ignoreIfs = mutable.Set.empty[String]
  val replaceIfs = mutable.Set.empty[String]
  var fromBooleanSym: Symbol = _
  var toFunc1Sym: Symbol = _
  var fromBranchesSym: Symbol = _
  var fromCasesSym: Symbol = _
  var dfValClsRef: TypeRef = _
  var dfEncodingRef: TypeRef = _
  var enumHackedUnapply: Symbol = _
  var dfcStack: List[Tree] = Nil

  override def prepareForDefDef(tree: DefDef)(using Context): Context =
    ContextArg.at(tree).foreach { t =>
      dfcStack = t :: dfcStack
    }
    ctx

  override def transformDefDef(tree: DefDef)(using Context): Tree =
    ContextArg.at(tree).foreach { t =>
      dfcStack = dfcStack.drop(1)
    }
    tree

  override def prepareForTypeDef(tree: TypeDef)(using Context): Context =
    ContextArg.at(tree).foreach { t =>
      dfcStack = t :: dfcStack
    }
    ctx

  override def transformTypeDef(tree: TypeDef)(using Context): Tree =
    ContextArg.at(tree).foreach { t =>
      dfcStack = dfcStack.drop(1)
    }
    tree

  override def transformApply(tree: Apply)(using Context): Tree =
    tree

  private def isHackedGuard(tree: Tree)(using Context): Boolean =
    tree match
      case Apply(Apply(Ident(n), List(dfCond)), List(dfc)) if n.toString == "BooleanHack" =>
        true
      case _ => false

  private def isHackedIf(tree: If)(using Context): Boolean =
    isHackedGuard(tree.cond)

  @tailrec private def isHackedIfRecur(tree: If)(using Context): Boolean =
    if (isHackedIf(tree)) true
    else
      tree.elsep match
        case tree: If => isHackedIfRecur(tree)
        case _        => false

  @tailrec private def ignoreElseIfRecur(tree: If)(using Context): Unit =
    ignoreIfs += tree.srcPos.show
    tree.elsep match
      case tree: If => ignoreElseIfRecur(tree)
      case _        => // done
  override def prepareForIf(tree: If)(using Context): Context =
    if (!ignoreIfs.contains(tree.srcPos.show) && isHackedIfRecur(tree))
      tree.elsep match
        case tree: If => ignoreElseIfRecur(tree)
        case _        => // do nothing
      replaceIfs += tree.srcPos.show
    ctx

  private def transformIfCond(condTree: Tree, dfcTree: Tree)(using
      Context
  ): Tree =
    condTree match
      case Apply(Apply(_, List(dfCondTree)), _) => dfCondTree
      case _ =>
        ref(fromBooleanSym)
          .appliedTo(condTree)
          .appliedTo(dfcTree)

  private def transformDFCaseBlock(tree: Tree, combinedTpe: Type)(using
      Context
  ): Tree =
    ref(toFunc1Sym)
      .appliedToType(combinedTpe)
      .appliedTo(tree)

  @tailrec private def transformIfRecur(
      tree: If,
      combinedTpe: Type,
      dfcTree: Tree,
      prevPairs: List[Tree]
  )(using
      Context
  ): (List[Tree], Tree) =
    val condTree = transformIfCond(tree.cond, dfcTree)
    val blockTree = transformDFCaseBlock(tree.thenp, combinedTpe)
    val pairs =
      mkTuple(List(condTree, blockTree)) :: prevPairs
    tree.elsep match
      case tree: If =>
        transformIfRecur(tree, combinedTpe, dfcTree, pairs)
      case Literal(Constant(_: Unit)) =>
        (pairs.reverse, ref(defn.NoneModule))
      case elseBlockTree =>
        val block = transformDFCaseBlock(elseBlockTree, combinedTpe)
        val someBlock = New(
          defn.SomeClass.typeRef.appliedTo(block.tpe),
          block :: Nil
        )
        (pairs.reverse, someBlock)
  end transformIfRecur

  override def transformIf(tree: If)(using Context): Tree =
    if (replaceIfs.contains(tree.srcPos.show))
      errorWhenToken(tree)
      debug("=======================")
      val dfcTree = dfcStack.head
      val combinedTpe = tree.tpe
      debug("DFC", dfcTree)
      debug(tree.show)
      debug(tree.srcPos.show)
      debug(tree)
      val (branchesVarArgs, elseOption) =
        transformIfRecur(tree, combinedTpe, dfcTree, Nil)
      val branches = mkList(branchesVarArgs)
      ref(fromBranchesSym)
        .appliedToType(combinedTpe)
        .appliedTo(branches, elseOption)
        .appliedTo(dfcTree)
    else tree

  object DFType:
    def apply(name: String, args: List[Type])(using Context): Type =
      AppliedType(
        requiredClassRef("dfhdl.core.DFType"),
        List(
          requiredClassRef(s"dfhdl.compiler.ir.$name"),
          if (args.isEmpty) requiredClassRef("dfhdl.core.NoArgs")
          else AppliedType(requiredClassRef("dfhdl.core.Args"), args)
        )
      )
    def unapply(arg: Type)(using Context): Option[(String, List[Type])] =
      arg.simple match
        case AppliedType(dfTypeCore, List(n, argsTp))
            if dfTypeCore.typeSymbol == requiredClass("dfhdl.core.DFType") =>
          val nameStr = n.typeSymbol.name.toString
          argsTp match
            case AppliedType(_, args) => Some(nameStr, args)
            case _                    => Some(nameStr, Nil)
        case _ => None
  end DFType
  object DFBoolOrBit:
    def unapply(arg: Type)(using Context): Boolean =
      arg match
        case DFType("DFBool$" | "DFBit$", Nil) => true
        case _                                 => false
  object DFBits:
    def unapply(arg: Type)(using Context): Option[Type] =
      arg match
        case DFType("DFBits", w :: Nil) => Some(w)
        case _                          => None
  object DFDecimal:
    def unapply(arg: Type)(using Context): Option[(Type, Type, Type)] =
      arg match
        case DFType("DFDecimal", s :: w :: f :: Nil) => Some(s, w, f)
        case _                                       => None
  object DFXInt:
    def unapply(arg: Type)(using Context): Option[(Boolean, Type)] =
      arg match
        case DFDecimal(
              ConstantType(Constant(sign: Boolean)),
              widthTpe,
              ConstantType(Constant(fractionWidth: Int))
            ) if fractionWidth == 0 =>
          Some(sign, widthTpe)
        case _ => None
  object DFUInt:
    def unapply(arg: Type)(using Context): Option[Type] =
      arg match
        case DFXInt(sign, widthTpe) if !sign => Some(widthTpe)
        case _                               => None
  object DFSInt:
    def unapply(arg: Type)(using Context): Option[Type] =
      arg match
        case DFXInt(sign, widthTpe) if sign => Some(widthTpe)
        case _                              => None
  object DFEnum:
    def unapply(arg: Type)(using Context): Option[Type] =
      arg match
        case DFType("DFEnum", e :: Nil) => Some(e)
        case _                          => None
  object DFStruct:
    def apply(t: Type)(using Context): Type =
      DFType("DFStruct", List(t))
    def unapply(arg: Type)(using Context): Option[Type] =
      arg match
        case DFType("DFStruct", t :: Nil) => Some(t)
        case _                            => None

  object DFVal:
    def apply(dfTypeTpe: Type)(using Context): Type =
      AppliedType(
        dfValClsRef,
        List(dfTypeTpe, requiredClassRef("dfhdl.core.ModifierAny"))
      )
    def unapply(
        selector: Tree
    )(using Context, ValDefGen): Option[Tree] =
      try
        val fixedTree = selector.tpe match
          // return the unmodified selector tree
          case DFVal(_) => Some(selector)
          case _ =>
            selector match
              // return the converted selector tree
              case DFTupleVal(tree)  => Some(tree)
              case DFStructVal(tree) => Some(tree)
              case _                 => None
        fixedTree.map(summon[ValDefGen].mkSelectValDef("sel", _))
      catch
        case e: IllegalArgumentException =>
          report.error(e.getMessage, selector.srcPos)
          None
    private def stripAndType(tpeOpt: Option[Type])(using Context): Option[Type] =
      tpeOpt.map(tpe =>
        tpe.simple match
          case AndType(t1, _) => t1
          case _              => tpe
      )
    def unapply(arg: Type)(using Context): Option[Type] =
      val ret = arg.simple match
        case AppliedType(t, List(dfType, _)) if t <:< dfValClsRef =>
          Some(dfType)
        case AppliedType(t, List(arg, mod))
            if t.typeSymbol.name.toString == "<>" && mod <:< requiredClassRef(
              "dfhdl.VAL"
            ) =>
          arg match
            case dfType @ DFType(_, _)      => Some(dfType)
            case DFTupleVal(DFVal(dfType))  => Some(dfType)
            case DFStructVal(DFVal(dfType)) => Some(dfType)
            case _                          => None
        case _ =>
          None
      stripAndType(ret)
    end unapply
  end DFVal

  object DFStructVal:
    def unapply(struct: Tree)(using Context): Option[Tree] =
      try
        struct.tpe match
          case DFStructVal(tpe) =>
            Some(FromCore.structToDFVal(tpe, struct))
          case _ => None
      catch
        case e: IllegalArgumentException =>
          report.error(e.getMessage, struct.srcPos)
          None
    def unapply(arg: Type)(using Context): Option[Type] =
      arg.simple match
        case fieldsTpe if fieldsTpe <:< requiredClassRef("dfhdl.core.DFStruct.Fields") =>
          val args = fieldsTpe.typeSymbol.asClass.paramAccessors.collect {
            case sym if sym.is(Flags.CaseAccessor) => fieldsTpe.memberInfo(sym)
          }
          val argsAreDFVal = args.map {
            case DFVal(_) => true
            case _        => false
          }
          if (args.isEmpty)
            throw new IllegalArgumentException(
              "No dataflow fields were found. A dataflow struct cannot be empty."
            )
          // all fields are dataflow values
          if (argsAreDFVal.forall(i => i)) Some(DFVal(DFStruct(fieldsTpe)))
          else
            throw new IllegalArgumentException(
              "Not all match selector structs fields are dataflow values."
            )
          end if
        case _ => None
  end DFStructVal

  object DFTupleVal:
    def unapply(tuple: Tree)(using Context): Option[Tree] =
      try
        tuple match
          // special casing a tuple match generated by anonymous functions like inside fold.
          // we know that this is happening when the argument name is starting with "x$"
          case Apply(_, Ident(x) :: _) if x.toString.startsWith("x$") => None
          case _ =>
            tuple.tpe match
              case DFTupleVal(tpe) =>
                Some(FromCore.structToDFVal(tpe, tuple))
              case _ => None
      catch
        case e: IllegalArgumentException =>
          report.error(e.getMessage, tuple.srcPos)
          None
    def unapply(arg: Type)(using Context): Option[Type] =
      arg.simple match
        case AppliedType(tpl, args) if tpl <:< defn.TupleTypeRef && args.nonEmpty =>
          val argsConv = args.map {
            case v @ DFVal(_)   => Some(v)
            case DFTupleVal(t)  => Some(t)
            case DFStructVal(t) => Some(t)
            case _              => None
          }
          // all tuple arguments are dataflow args
          if (argsConv.forall(_.isDefined))
            val dfType = DFStruct(AppliedType(tpl, argsConv.flatten))
            Some(DFVal(dfType))
          // all tuple arguments are NOT dataflow args
          else if (argsConv.forall(_.isEmpty)) None
          else
            throw new IllegalArgumentException(
              "Not all match selector tuple fields are dataflow values."
            )
          end if
        case _ => None
      end match
    end unapply
  end DFTupleVal

  private def transformLiteralCasePattern(
      selector: Tree,
      constPat: Constant,
      errPos: util.SrcPos
  )(using
      Context
  ): Tree =
    def patternSingleton: Tree = FromCore.patternSingleton(selector, constPat)
    val DFVal(dfTypeTpe) = selector.tpe: @unchecked
    (dfTypeTpe, constPat) match
      case (DFXInt(signed, widthTpe), Constant(i: Int)) if i < 0 && !signed =>
        report.error(
          s"Cannot compare a signed literal value with an unsigned dataflow variable.\nAn explicit conversion must be applied.",
          errPos
        )
        EmptyTree
      case (
            DFXInt(signed, ConstantType(Constant(width: Int))),
            Constant(i: Int)
          ) if i.bitsWidth(signed) > width =>
        report.error(
          s"Cannot compare a dataflow value (width = $width) with a Scala `Int` argument that is wider (width = ${i
              .bitsWidth(signed)}).\nAn explicit conversion must be applied.",
          errPos
        )
        EmptyTree
      case (DFXInt(signed, widthTpe), Constant(i: Int)) => patternSingleton
      case (DFBoolOrBit(), Constant(_: Boolean))        => patternSingleton
      case (DFBoolOrBit(), Constant(i: Int)) if i == 0 | i == 1 =>
        patternSingleton
      case (selectorTpe, constPat) =>
        report.error(
          s"Unsupported literal ${constPat.show} for the dataflow variable type ${selectorTpe.show}",
          errPos
        )
        EmptyTree
    end match
  end transformLiteralCasePattern

  class ValDefGen:
    private var bindMap = immutable.ListMap.empty[Name, Tree]
    private var valDefs = List.empty[ValDef]
    private val bindsReplacer = new TreeMap():
      override def transform(tree: tpd.Tree)(using Context): Tree =
        tree match
          case Ident(n) if bindMap.contains(n) => bindMap(n)
          case _ =>
            super.transform(tree)
    def mkSelectValDef(name: String, tree: Tree)(using
        Context
    ): Tree =
      val uniqueName = NameKinds.UniqueName.fresh(s"${name}_plugin".toTermName)
      val valDef = SyntheticValDef(uniqueName, tree)
      val select = ref(valDef.symbol)
      valDefs = valDef :: valDefs
      select
    def bind(name: Name, tree: Tree)(using
        Context
    ): Tree =
      val ret =
        mkSelectValDef(s"bind_${name}", tree)
      bindMap = bindMap + (name -> ret)
      ret
    def replaceBinds(tree: Tree)(using Context): Tree =
      val ret = bindsReplacer.transform(tree)
      ret
    def getBinds: immutable.ListMap[Name, Tree] = bindMap
    def getValDefs: List[ValDef] = valDefs.reverse
    def clearBinds(): Unit =
      bindMap = immutable.ListMap()
  end ValDefGen

  private def transformDFCaseGuard(guardTree: Tree)(using
      Context
  ): Tree =
    guardTree match
      case Apply(Apply(_, List(dfGuardTree)), _) =>
        mkSome(dfGuardTree)
      case _ if !guardTree.isEmpty =>
        mkSome(
          ref(fromBooleanSym)
            .appliedTo(guardTree)
            .appliedTo(dfcStack.head)
        )
      case _ =>
        ref(defn.NoneModule.termRef)

  object FromCore:
    private val fullPath = "dfhdl.core.__For_Plugin"
    def selectMethod(methodName: String)(using Context): Tree =
      ref(requiredMethod(s"$fullPath.$methodName"))
    def structToDFVal(retTpe: Type, productTree: Tree)(using Context): Tree =
      selectMethod("structToDFVal")
        .appliedToType(retTpe)
        .appliedTo(productTree)
        .appliedTo(dfcStack.head)
    def structDFValSelect(retTpe: Type, dfValTree: Tree, fieldName: String)(using
        Context
    ): Tree =
      selectMethod("structDFValSelect")
        .appliedToType(retTpe)
        .appliedToArgs(List(dfValTree, Literal(Constant(fieldName))))
        .appliedTo(dfcStack.head)
    def extractValDcl(selectorTree: Tree, extractName: String)(using
        Context
    ): Tree =
      selectMethod("extractValDcl")
        .appliedToType(selectorTree.tpe.widen)
        .appliedToArgs(List(selectorTree, Literal(Constant(extractName))))
        .appliedTo(dfcStack.head)
    def bindVal(selectorTree: Tree, bindName: String)(using Context): Tree =
      selectMethod("bindVal")
        .appliedToType(selectorTree.tpe.widen)
        .appliedToArgs(List(selectorTree, Literal(Constant(bindName))))
        .appliedTo(dfcStack.head)
    def bindValRange(
        selectorTree: Tree,
        bindName: String,
        relBitHigh: Int,
        relBitLow: Int
    )(using Context): Tree =
      selectMethod("bindValRange")
        .appliedToType(selectorTree.tpe.widen)
        .appliedToArgs(
          List(
            selectorTree,
            Literal(Constant(bindName)),
            Literal(Constant(relBitHigh)),
            Literal(Constant(relBitLow))
          )
        )
        .appliedTo(dfcStack.head)
    def forcedAssign(toValTree: Tree, fromValTree: Tree)(using Context): Tree =
      selectMethod("forcedAssign")
        .appliedToArgs(List(toValTree, fromValTree))
        .appliedTo(dfcStack.head)
    def patternStruct(name: String, patternTrees: List[Tree])(using
        Context
    ): Tree =
      selectMethod("patternStruct")
        .appliedToArgs(List(Literal(Constant(name)), mkList(patternTrees)))
    def patternSingleton(selector: Tree, constPat: Constant)(using
        Context
    ): Tree =
      patternSingleton(selector, Literal(constPat))
    def patternSingleton(selector: Tree, constTree: Tree)(using Context): Tree =
      selectMethod("patternSingleton").appliedToArgs(List(selector, constTree))
    def patternBind(bindValTree: Tree, patternTree: Tree)(using Context): Tree =
      selectMethod("patternBind")
        .appliedToArgs(List(bindValTree, patternTree))
        .appliedTo(dfcStack.head)
    def patternBindSI(
        opTree: Tree,
        partTrees: List[Tree],
        bindValTrees: List[Tree]
    )(using
        Context
    ): Tree =
      selectMethod("patternBindSI")
        .appliedToArgs(
          List(opTree, mkList(partTrees), mkList(bindValTrees))
        )
        .appliedTo(dfcStack.head)
    def unapplySeq(tree: Tree, argTree: Tree)(using Context): Tree =
      tree
        .select("unapplySeq".toTermName)
        .appliedTo(argTree)
        .appliedTo(dfcStack.head)
    def patternSingletonSI(siTree: Tree)(using Context): Tree =
      selectMethod("patternSingletonSI").appliedTo(siTree)
    def patternCatchAll(using Context): Tree = selectMethod("patternCatchAll")
    def patternAlternative(patternTrees: List[Tree])(using Context): Tree =
      selectMethod("patternAlternative").appliedTo(mkList(patternTrees))
    def dfMatchFromCases(
        retTpe: Type,
        selectorTree: Tree,
        caseTupleTrees: List[Tree],
        forceAnonymous: Boolean
    )(using Context): Tree =
      ref(fromCasesSym)
        .appliedToType(retTpe)
        .appliedTo(
          selectorTree,
          mkList(caseTupleTrees),
          Literal(Constant(forceAnonymous))
        )
        .appliedTo(dfcStack.head)
    end dfMatchFromCases

  end FromCore

  object Pattern:
    object Tuple:
      def unapply(arg: UnApply)(using Context): Option[List[Tree]] =
        arg match
          case UnApply(TypeApply(Select(Ident(tplName), _), _), _, patterns)
              if tplName.toString.startsWith("Tuple") =>
            Some(patterns)
          case _ => None
    object SEV:
      def unapply(arg: UnApply)(using Context): Option[Literal] =
        arg match
          case UnApply(fun, List(), List(lit: Literal))
              if fun.symbol == requiredMethod("dfhdl.all.unapply") =>
            Some(lit)
          case _ => None
    object Enum:
      def unapply(arg: UnApply | Select | Ident)(using Context): Option[Tree] =
        arg match
          case unapply @ UnApply(TypeApply(Apply(_, List(arg)), _), _, _)
              if unapply.fun.symbol == enumHackedUnapply =>
            Some(arg)
          case arg: (Select | Ident) if arg.tpe <:< dfEncodingRef =>
            Some(arg)
          case _ => None
    object SI:
      def unapply(arg: UnApply)(using
          Context
      ): Option[(Block, List[Tree], Tree)] =
        arg match
          case UnApply(select: Select, _, binds) =>
            select match
              case Select(tree @ Block(List(TypeDef(_, template)), _), _) =>
                val Template(_, _, _, List(defdef)) = template: @unchecked
                val DefDef(_, _, _, rhs: Tree @unchecked) = defdef: @unchecked
                Some(tree, binds, rhs.underlying)
              case _ => None
          case _ => None
      object Binds:
        def unapply(rhs: Tree)(using Context): Option[List[Tree]] =
          rhs match
            case Apply(_, List(Apply(_, List(Typed(SeqLiteral(elems, _), _))))) =>
              Some(elems)
            case _ => None
    end SI
    object Struct:
      def unapply(arg: UnApply)(using Context): Option[(Type, List[Tree])] =
        arg match
          case UnApply(fun @ Select(_, _), _, patterns: List[Tree]) =>
            val resType = fun.tpe.simple match
              case mt: MethodType => mt.resType
            Some(resType, patterns)
          case _ => None
  end Pattern

  private def transformDFCasePattern(
      selectorTree: Tree,
      patternTree: Tree,
      prefixBindName: String
  )(using
      ctx: Context,
      valDefGen: ValDefGen
  ): Tree = boundary:
    val DFVal(dfTypeTpe) = selectorTree.tpe: @unchecked
    patternTree match
      case Pattern.Tuple(patterns) =>
        dfTypeTpe match
          case DFStruct(AppliedType(tpl, selectorTpes)) =>
            if (selectorTpes.length != patterns.length)
              report.error(
                s"The number of patterns in the pattern (${patterns.length}) tuple does not match the number of fields in the selector (${selectorTpes.length})",
                patternTree.srcPos
              )
            val dfPatterns =
              selectorTpes
                .lazyZip(patterns)
                .zipWithIndex
                .map { case ((s, p), i) =>
                  transformDFCasePattern(
                    FromCore.structDFValSelect(s, selectorTree, s"_${i + 1}"),
                    p,
                    prefixBindName
                  )
                }
                .toList
            FromCore.patternStruct("", dfPatterns)
          case _ =>
            report.error(
              s"Found a tuple pattern but the match selector is not a tuple.",
              patternTree.srcPos
            )
            EmptyTree
        end match
      case Literal(const) =>
        transformLiteralCasePattern(selectorTree, const, patternTree.srcPos)
      // unapply of "all" bits literal
      case Pattern.SEV(lit) =>
        dfTypeTpe match
          case DFBits(_) => // ok
          case _ =>
            report.error(
              "`all` pattern is allowed for a Bits dataflow value only.",
              patternTree.srcPos
            )
        FromCore.patternSingleton(selectorTree, lit)
      // hacked unapply for enum enumerations
      case Pattern.Enum(arg) =>
        val DFEnum(enumTpe) = dfTypeTpe: @unchecked
        if (arg.tpe <:< enumTpe) FromCore.patternSingleton(selectorTree, arg)
        else
          report.error(
            s"""Wrong enum entry type.
                  |Expecting: ${enumTpe.show}
                  |Found: ${arg.tpe.show}""".stripMargin,
            arg.srcPos
          )
          EmptyTree
      // token string interpolation
      case Pattern.SI(block, binds, rhs) =>
        dfTypeTpe match
          case DFBits(_) | DFXInt(_, _) => // ok
          case _ =>
            report.error(
              "String interpolation pattern is only allowed for Bits, UInt, or SInt dataflow values.",
              patternTree.srcPos
            )
            break(EmptyTree)
        rhs match
          case Pattern.SI.Binds(elems) =>
            val selectorWidth = dfTypeTpe match
              case DFBits(ConstantType(Constant(w: Int))) => w
              case DFUInt(ConstantType(Constant(w: Int))) => w
              case _ =>
                report.error(
                  "Value extraction with a string interpolation pattern is only allowed for Bits or UInt dataflow values.",
                  patternTree.srcPos
                )
                break(EmptyTree)
            val Literal(Constant(op: String)) = elems.head: @unchecked
            val fullSI =
              Seq(elems.drop(1), binds)
                .flatMap(_.zipWithIndex)
                .sortBy(_._2)
                .map(_._1)
            var relBitHigh: Int = selectorWidth
            var bindSelTrees: List[Tree] = Nil
            fullSI.foreach {
              case Literal(Constant(e: String)) =>
                val partWidth = op match
                  case "b" => e.length
                  case "h" => e.length * 4
                relBitHigh = relBitHigh - partWidth
              case bindTree: Bind =>
                bindTree.tpe.simple match
                  case AndType(_, DFVal(DFBits(widthTpe))) =>
                    widthTpe match
                      case ConstantType(Constant(partWidth: Int)) if partWidth > 0 =>
                        val relBitLow = relBitHigh - partWidth + 1
                        val newBindSel = valDefGen.bind(
                          bindTree.name,
                          FromCore.bindValRange(
                            selectorTree,
                            s"$prefixBindName${bindTree.name}",
                            relBitHigh,
                            relBitLow
                          )
                        )
                        bindSelTrees = newBindSel :: bindSelTrees
                        relBitHigh = relBitHigh - partWidth
                      case _ =>
                        report.error(
                          s"The bind `${bindTree.name}` must have a known constant positive width, but found: ${widthTpe.show}",
                          bindTree.srcPos
                        )
                        break(EmptyTree)
                  case _ =>
                    report.error(
                      s"The bind `${bindTree.name}` must have a Bits value type annotation `: B[<width>]`",
                      bindTree.srcPos
                    )
                    break(EmptyTree)

            }
            if (relBitHigh != 0)
              report.error(
                s"""Cannot compare a value of ${selectorWidth} bits width (LHS) to a value of ${selectorWidth - relBitHigh} bits width (RHS).
                   |An explicit conversion must be applied.""".stripMargin,
                patternTree.srcPos
              )
              break(EmptyTree)
            // success!
            FromCore.patternBindSI(
              elems.head,
              elems.drop(1),
              bindSelTrees.reverse
            )
          case _ =>
            FromCore.patternSingletonSI(
              FromCore.unapplySeq(block, selectorTree)
            )
        end match
      case Typed(tree, _) =>
        transformDFCasePattern(selectorTree, tree, prefixBindName)
      // catch all
      case Ident(i) if i.toString == "_" =>
        FromCore.patternCatchAll
      // catch all with name bind
      case Bind(n, boundPattern) =>
        val newBindSel =
          valDefGen.bind(
            n,
            FromCore.bindVal(selectorTree, s"$prefixBindName$n")
          )
        val dfPattern =
          transformDFCasePattern(newBindSel, boundPattern, prefixBindName)
        // finally, construct the dataflow bounded pattern
        FromCore.patternBind(newBindSel, dfPattern)
      // union of alternatives
      case Alternative(list) =>
        FromCore.patternAlternative(
          list.map(transformDFCasePattern(selectorTree, _, prefixBindName))
        )
      // struct pattern
      case Pattern.Struct(resType, patterns) =>
        dfTypeTpe match
          // TODO: change to <:< type check to support tagged unions, but need to consider sub-values first
          case DFStruct(t: Type) if resType =:= t =>
            val fieldNamesAndTypes =
              t.typeSymbol.asClass.paramAccessors.collect {
                case sym if sym.is(Flags.CaseAccessor) =>
                  (sym.name.toString, t.memberInfo(sym))
              }
            val dfPatterns =
              fieldNamesAndTypes
                .lazyZip(patterns)
                .map { case ((fn, ft), p) =>
                  transformDFCasePattern(
                    FromCore.structDFValSelect(ft, selectorTree, fn),
                    p,
                    prefixBindName
                  )
                }
            FromCore.patternStruct(t.typeSymbol.name.toString, dfPatterns)
          case _ =>
            report.error(
              s"Invalid pattern of type ${resType.show} for the given selector.",
              patternTree.srcPos
            )
            EmptyTree
        end match
      // unknown pattern
      case _ =>
        report.error(s"Unknown pattern:\n${patternTree.show}\n$patternTree")
        EmptyTree
    end match

  private def transformDFCase(selector: Tree, tree: CaseDef, combinedTpe: Type)(using
      ctx: Context,
      valDefGen: ValDefGen
  ): Tree =
    val patternTree = transformDFCasePattern(selector, tree.pat, "")
    val guardTree = transformDFCaseGuard(valDefGen.replaceBinds(tree.guard))
    val blockTree =
      transformDFCaseBlock(valDefGen.replaceBinds(tree.body), combinedTpe)
    valDefGen.clearBinds()
    mkTuple(List(patternTree, guardTree, blockTree))
  end transformDFCase

  private def errorWhenToken(tree: Tree)(using Context): Unit =
    val control = tree match
      case _: If    => "if"
      case _: Match => "match"
    tree.tpe.simple match
      case AppliedType(tycon, _)
          if tycon <:< requiredClassRef(
            "dfhdl.core.DFToken"
          ) =>
        report.error(
          s"This dataflow `$control` is missing an explicit type annotation.",
          tree.srcPos
        )
      case _ =>
  end errorWhenToken

  override def transformMatch(tree: Match)(using Context): Tree =
    given valDefGen: ValDefGen = new ValDefGen
    tree.selector match
      case DFVal(newSelector) =>
        debug("Found DFMatch")
        val extractorMatch =
          tree.tpe <:< defn.TupleTypeRef && tree.cases.length == 1 && tree.cases.head.guard.isEmpty
        if (extractorMatch)
          val patternTree =
            transformDFCasePattern(newSelector, tree.cases.head.pat, "_")
          val guardTree = ref(defn.NoneModule.termRef)
          val assignmentTrees = valDefGen.getBinds.map { (n, sel) =>
            val dcl = valDefGen.bind(n, FromCore.extractValDcl(sel, n.toString))
            FromCore.forcedAssign(dcl, sel)
          }.toList
          val blockTree =
            transformDFCaseBlock(
              Block(assignmentTrees, Literal(Constant(()))),
              defn.UnitType
            )
          val cases = List(
            mkTuple(List(patternTree, guardTree, blockTree))
          )
          val tupleRet = valDefGen.replaceBinds(tree.cases.head.body)
          val dfMatch =
            FromCore.dfMatchFromCases(defn.UnitType, newSelector, cases, true)
          Block(valDefGen.getValDefs :+ dfMatch, tupleRet)
        else
          errorWhenToken(tree)
          val cases =
            tree.cases.map(c => transformDFCase(newSelector, c, tree.tpe))
          val dfMatch =
            FromCore.dfMatchFromCases(tree.tpe, newSelector, cases, false)
          Block(valDefGen.getValDefs, dfMatch)
        end if
      case _ =>
        tree
    end match
  end transformMatch

  override def prepareForApply(tree: Apply)(using Context): Context =
    /*
      TODO: this causes an error after move to @precise.
      Don't remember why this was required to begin with :(
      Commented out. Will revisit in the future.
     */
//    val sym = tree.symbol
//    val symName = sym.name.toString()
//    sym.signature.paramsSig
//      .drop(1)
//      .map(_.toString)
//      .lazyZip(tree.args)
//      .collectFirst {
//        case (sig, arg) if sig == "dfhdl.core.DFVal" && arg.tpe <:< dfEncodingRef => arg
//      }
//      .foreach(t =>
//        report.error(
//          s"""value $symName is not a member of dfhdl.core.DFEncoding.
//             |Note: this error was forced by the DFHDL compiler plugin.""".stripMargin,
//          tree.srcPos
//        )
//      )
    ctx
  end prepareForApply

  override def prepareForUnit(tree: Tree)(using Context): Context =
    super.prepareForUnit(tree)
    ignoreIfs.clear()
    replaceIfs.clear()
    fromBooleanSym = requiredMethod("dfhdl.core.__For_Plugin.fromBoolean")
    toFunc1Sym = requiredMethod("dfhdl.core.__For_Plugin.toFunc1")
    fromBranchesSym = requiredMethod("dfhdl.core.DFIf.fromBranches")
    fromCasesSym = requiredMethod("dfhdl.core.DFMatch.fromCases")
    dfValClsRef = requiredClassRef("dfhdl.core.DFVal")
    dfEncodingRef = requiredClassRef("dfhdl.core.DFEncoding")
    enumHackedUnapply = requiredMethod("dfhdl.unapply")
    ctx
end CustomControlPhase
