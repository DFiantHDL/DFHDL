package DFiant.plugin

import dotty.tools.dotc.*
import plugins.*
import core.*
import Contexts.*
import Symbols.*
import Flags.*
import SymDenotations.*
import Decorators.*
import ast.Trees.*
import ast.tpd
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
import collection.mutable
import annotation.tailrec

extension (value: BigInt)
  def bitsWidth(signed: Boolean): Int =
    if (value > 0)
      if (signed) value.bitLength + 1 else value.bitLength
    else if (value == 0)
      if (signed) 2 else 1
    else if (value == -1) 2
    else value.bitLength + 1 // value < 0
extension (value: Int)
  def bitsWidth(signed: Boolean): Int = BigInt(value).bitsWidth(signed)

class CustomIfPhase(setting: Setting) extends CommonPhase:
  import tpd._

  val phaseName = "CustomIf"
  override val debugFilter: String => Boolean =
    _.contains("DFMatchSpec.scala")
  override val runsAfter = Set(transform.Pickler.name)
  override val runsBefore = Set("MetaContextGen")
  val ignoreIfs = mutable.Set.empty[String]
  val replaceIfs = mutable.Set.empty[String]
  var fromBooleanSym: Symbol = _
  var toFunc1Sym: Symbol = _
  var toTuple2Sym: Symbol = _
  var toTuple3Sym: Symbol = _
  var fromBranchesSym: Symbol = _
  var fromCasesSym: Symbol = _
  var dfValClsRef: TypeRef = _
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
      case Apply(Apply(Ident(n), List(dfCond)), List(dfc))
          if n.toString == "BooleanHack" =>
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

  private def transformBlock(tree: Tree, combinedTpe: Type)(using
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
    val blockTree = transformBlock(tree.thenp, combinedTpe)
    val pairs =
      ref(toTuple2Sym)
        .appliedToTypes(List(condTree.tpe, blockTree.tpe))
        .appliedToArgs(List(condTree, blockTree)) :: prevPairs
    tree.elsep match
      case tree: If =>
        transformIfRecur(tree, combinedTpe, dfcTree, pairs)
      case Literal(Constant(_: Unit)) =>
        (pairs.reverse, ref(defn.NoneModule))
      case elseBlockTree =>
        val block = transformBlock(elseBlockTree, combinedTpe)
        val someBlock = New(
          defn.SomeClass.typeRef.appliedTo(block.tpe),
          block :: Nil
        )
        (pairs.reverse, someBlock)
  end transformIfRecur

  override def transformIf(tree: If)(using Context): Tree =
    if (replaceIfs.contains(tree.srcPos.show))
      debug("=======================")
      val dfcTree = dfcStack.head
      val combinedTpe = tree.tpe
      debug("DFC", dfcTree)
      debug(tree.show)
      debug(tree.srcPos.show)
      debug(tree)
      val (branchesVarArgs, elseOption) =
        transformIfRecur(tree, combinedTpe, dfcTree, Nil)
      val branches = mkList(branchesVarArgs, TypeTree(defn.AnyType))
      ref(fromBranchesSym)
        .appliedToType(combinedTpe)
        .appliedTo(branches, elseOption)
        .appliedTo(dfcTree)
    else tree

  object DFType:
    def unapply(arg: Type)(using Context): Option[(String, List[Type])] =
      arg.dealias match
        case AppliedType(dfTypeCore, List(n, AppliedType(_, args)))
            if dfTypeCore.typeSymbol == requiredClass("DFiant.core.DFType") =>
          Some(n.typeSymbol.name.toString, args)
        case _ => None
  object DFDecimal:
    def unapply(arg: Type)(using Context): Option[(Type, Type, Type)] =
      arg match
        case DFType("DFDecimal", s :: w :: f :: Nil) =>
          Some(s, w, f)
        case _ => None
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
        case DFXInt(sign, widthTpe) if !sign =>
          Some(widthTpe)
        case _ => None
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
  object DFVal:
    def unapply(arg: Tree)(using Context): Option[Tree] =
      arg.tpe match
        case DFVal(_, _) => Some(arg)
        case _           => None
    def unapply(arg: Type)(using Context): Option[(Type, Type)] =
      arg.underlyingIfProxy.dealias match
        case AppliedType(t, List(dfType, mod)) if t <:< dfValClsRef =>
          Some(dfType, mod)
        case _ => None

//  object DFTupleVal:
//    def unapply(arg: Tree)(using Context): Option[List[Type]] =
//      unapply(arg.tpe.underlyingIfProxy)
//    def unapply(arg: Type)(using Context): Option[List[Type]] =
//      arg match
//        case AppliedType(tpl, args) if tpl <:< defn.TupleTypeRef =>
//          args.map {
//            case v @ DFVal(_,_) => v
//            case _ => false
//          })
//        case _ => None

  private def transformLiteralCasePattern(
      selector: Tree,
      constPat: Constant,
      errPos: util.SrcPos
  )(using
      Context
  ): Tree =
    val DFVal(dfTypeTpe, _) = selector.tpe
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
      case (DFXInt(signed, widthTpe), Constant(i: Int)) =>
        // Construct a singleton pattern
        ref(requiredMethod("DFiant.core.__For_Plugin.patternSingletonInt"))
          .appliedToArgs(
            List(selector, Literal(constPat))
          )
      case (selectorTpe, constPat) =>
        report.error(
          s"Unsupported literal ${constPat.show} for the dataflow variable type ${selectorTpe.show}",
          errPos
        )
        EmptyTree
    end match
  end transformLiteralCasePattern

  private def mkSome(tree: Tree)(using Context): Tree =
    ref(requiredMethod("scala.Some.apply"))
      .appliedToType(tree.tpe)
      .appliedTo(tree)

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

  private def transformDFCasePattern(selector: Tree, tree: Tree)(using
      Context
  ): Tree =
    tree match
      case Literal(const) =>
        debug("Found literal pattern")
        transformLiteralCasePattern(
          selector,
          const,
          tree.srcPos
        )
      // hacked unapply for enum enumerations
      case unapply @ UnApply(TypeApply(Apply(_, List(arg)), _), _, _)
          if unapply.fun.symbol == enumHackedUnapply =>
        debug("Found enum literal pattern")
        val DFVal(DFEnum(enumTpe), _) = selector.tpe
        if (arg.tpe <:< enumTpe)
          ref(
            requiredMethod("DFiant.core.__For_Plugin.patternSingletonEnum")
          )
            .appliedTo(selector, arg)
        else
          report.error(
            s"""Wrong enum entry type.
                  |Expecting: ${enumTpe.show}
                  |Found: ${arg.tpe.show}""".stripMargin,
            arg.srcPos
          )
          EmptyTree
      // token string interpolation
      case UnApply(Select(tree @ Block(List(TypeDef(_, _)), _), _), _, _) =>
        debug("Found token string interpolation")
        ref(requiredMethod("DFiant.core.__For_Plugin.patternSingletonSI"))
          .appliedTo(
            tree
              .select("unapplySeq".toTermName)
              .appliedTo(selector)
              .appliedTo(dfcStack.head)
          )
      case Typed(tree, _) =>
        transformDFCasePattern(selector, tree)
      // catch all
      case Ident(i) if i.toString == "_" =>
        ref(requiredMethod("DFiant.core.__For_Plugin.patternCatchAll"))
      // catch all with name bind
      case Bind(n, Ident(i)) if i.toString == "_" =>
        ???
      // named and typed bind
      case Bind(n, Typed(Ident(i), tpt)) if i.toString == "_" =>
        ???
      // union of alternatives
      case Alternative(list) =>
        debug("Found pattern alternatives")
        ref(requiredMethod("DFiant.core.__For_Plugin.patternAlternative"))
          .appliedTo(
            mkList(
              list.map(transformDFCasePattern(selector, _)),
              TypeTree(defn.AnyType)
            )
          )

      // unknown pattern
      case _ =>
        debug(s"Unknown pattern: ${tree.show}")
        debug(tree)
        ???
    end match
  end transformDFCasePattern

  private def transformDFCase(selector: Tree, tree: CaseDef, combinedTpe: Type)(
      using Context
  ): Tree =
    val patternTree = transformDFCasePattern(selector, tree.pat)
    val guardTree = transformDFCaseGuard(tree.guard)
    val blockTree = transformBlock(tree.body, combinedTpe)
    ref(toTuple3Sym)
      .appliedToTypes(List(patternTree.tpe, guardTree.tpe, blockTree.tpe))
      .appliedToArgs(List(patternTree, guardTree, blockTree))

  override def transformMatch(tree: Match)(using Context): Tree =
    tree.selector match
      case DFVal(_) =>
        debug("Found DFMatch")
        val casesVarArgs =
          tree.cases.map(c => transformDFCase(tree.selector, c, tree.tpe))
        val cases = mkList(casesVarArgs, TypeTree(casesVarArgs.head.tpe))
        ref(fromCasesSym)
          .appliedToType(tree.tpe)
          .appliedTo(tree.selector, cases)
          .appliedTo(dfcStack.head)
      case _ =>
        tree
  end transformMatch
  override def prepareForUnit(tree: Tree)(using Context): Context =
    super.prepareForUnit(tree)
    ignoreIfs.empty
    replaceIfs.empty
    fromBooleanSym = requiredMethod("DFiant.core.__For_Plugin.fromBoolean")
    toFunc1Sym = requiredMethod("DFiant.core.__For_Plugin.toFunc1")
    toTuple2Sym = requiredMethod("DFiant.core.__For_Plugin.toTuple2")
    toTuple3Sym = requiredMethod("DFiant.core.__For_Plugin.toTuple3")
    fromBranchesSym = requiredMethod("DFiant.core.DFIf.fromBranches")
    fromCasesSym = requiredMethod("DFiant.core.DFMatch.fromCases")
    dfValClsRef = requiredClassRef("DFiant.core.DFVal")
    enumHackedUnapply = requiredMethod("DFiant.unapply")
    ctx
end CustomIfPhase
