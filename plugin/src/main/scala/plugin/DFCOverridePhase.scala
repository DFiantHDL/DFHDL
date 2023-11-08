package dfhdl.plugin

import dotty.tools.dotc._

import plugins._

import core._
import Contexts._
import Symbols._
import Flags._
import SymDenotations._

import Decorators._
import ast.Trees._
import ast.{tpd, untpd, TreeTypeMap}
import StdNames.nme
import Names._
import Constants.Constant
import Types._
import scala.language.implicitConversions
import collection.mutable
import annotation.tailrec
import Comments.CommentsContext

class DFCOverridePhase(setting: Setting) extends CommonPhase:
  import tpd._

  val phaseName = "DFCOverride"
  // override val debugFilter: String => Boolean = _.contains("PluginSpec.scala")

  override val runsAfter = Set(transform.Pickler.name)
  override val runsBefore = Set("MetaContextGen")

  extension (tree: ValOrDefDef)(using Context)
    def dfcFuncTpeOpt: Option[Type] =
      tree.tpt.tpe.dfcFuncTpeOpt
  extension (tree: NamedDefTree)(using Context)
    def newDefSymbol(newOwner: Symbol, newInfo: Type): TermSymbol =
      val sym = tree.symbol
      val retSym = newSymbol(
        newOwner,
        tree.name,
        sym.flags,
        newInfo,
        coord = sym.coord,
        nestingLevel = sym.nestingLevel
      )
      retSym.annotations = sym.annotations.filterNot(
        _.tree.tpe.typeSymbol == defn.ContextResultCountAnnot
      )
      ctx.docCtx.foreach(c => c.addDocstring(retSym, c.docstring(sym)))
      retSym.asTerm
    end newDefSymbol
  end extension

  extension (sym: Symbol)(using Context)
    def ownerOpt: Option[Symbol] =
      if (sym.denot eq NoDenotation) None
      else Some(sym.owner)

  def check(tree: Tree)(using Context): Tree =
    new TreeMap():
      override def transform(tree: Tree)(using Context): Tree =
        tree match
          case vd: ValDef =>
            debug(
              s"${vd.symbol}, ${vd.symbol.ownerOpt}, ${vd.tpe.show}:   ${vd.symbol.flagsString}"
            )
          case _ =>
        super.transform(tree)
    .transform(tree)

  def checkRemainingContextDFVals(tree: Tree)(using Context): Tree =
    new TreeMap():
      override def transform(tree: Tree)(using Context): Tree =
        tree match
          case Block(List(anonDef: DefDef), closure: Closure) =>
          case _ =>
            if (
              tree.tpe.dfcFuncTpeOptRecur.nonEmpty || tree.symbol.exists && tree.symbol.info.dfcFuncTpeOptRecur.nonEmpty
            )
              debug("----------------------------------------------")
              debug(s"Found unexpected context!")
              debug(s"Type:        ${tree.tpe.show}")
              if (tree.symbol.exists)
                debug(s"Symbol:      ${tree.symbol}")
                debug(s"Symbol Info: ${tree.symbol.info.show}")
              debug(tree.show)
              debug(tree)
        super.transform(tree)
      end transform
    .transform(tree)

  private def transformDFCFunc(tree: Tree)(using Context): Tree =
    val symMap = mutable.Map.empty[Symbol, Symbol]
    val ownerMap = mutable.Map.empty[Symbol, Symbol]
    var dfcStack: List[Tree] = List(ref(requiredMethod("dfhdl.core.DFC.empty")))
    def updatedDFC = dfcStack.head
    val replaceDFCMap = mutable.Map.empty[Symbol, Tree]

    def withDFCIn[T](tree: DefDef | TypeDef)(block: => T): T =
      ContextArg.at(tree) match
        case Some(dfc) =>
          dfcStack = dfc :: dfcStack
          val ret = block
          dfcStack = dfcStack.drop(1)
          ret
        case None => block
    val r1 = new TreeMap():
      override def transform(tree: Tree)(using Context): Tree =
        tree match
          case vd: ValDef =>
            vd.tpt.tpe.dfcFuncTpeOptRecur match
              case Some(vdTpe) =>
                val vdRHS = vd.rhs
                val vdSym = vd.symbol
                val retSym = vd.newDefSymbol(vdSym.owner, vdTpe)
                val retRHS = vdRHS match
                  case block @ Block(List(anonDef: DefDef), closure: Closure) =>
                    val updatedAnonDef = cpy.DefDef(anonDef)(rhs = transform(anonDef.rhs))
                    val updatedBlock = Block(List(updatedAnonDef), closure)
                    Apply(Select(updatedBlock, nme.apply), List(updatedDFC))
                  case _ => vdRHS
                symMap += vdSym -> retSym
                ValDef(retSym, retRHS, inferred = false).withSpan(vd.span)
              case _ => super.transform(vd)
          case dd: DefDef =>
            withDFCIn(dd):
              dd.dfcFuncTpeOpt match
                case Some(ddTpe) =>
                  val ddRHS = dd.rhs
                  val ddSym = dd.symbol
                  val anonDef = ddRHS match
                    case Block(List(anonDef: DefDef), _: Closure)           => anonDef
                    case Typed(Block(List(anonDef: DefDef), _: Closure), _) => anonDef
                    case _                                                  => ???
                  val retParamClauses = dd.paramss ++ anonDef.paramss
                  val retParamss = retParamClauses.map(_.map {
                    case vd: ValDef =>
                      vd.dfcFuncTpeOpt match
                        case Some(resTpe) =>
                          val newSym = vd.newDefSymbol(ddSym, resTpe)
                          symMap += vd.symbol -> newSym
                          newSym
                        case None => vd.symbol
                    case td: TypeDef => td.symbol
                  })
                  val retInfo = NamerOps.methodType(retParamss, ddTpe)
                  val retSym = dd.newDefSymbol(ddSym.owner, retInfo)
                  val retRHS = withDFCIn(anonDef):
                    transform(anonDef.rhs)
                  ownerMap += anonDef.symbol -> retSym
                  ownerMap += ddSym -> retSym
                  // debug(s"Map: ${vdSym}, ${vdSym.hashCode()} -> ${retSym}, ${retSym.hashCode()}")
                  DefDef(retSym.asTerm, retParamss, ddTpe, retRHS).withSpan(dd.span)
                case None =>
                  val hasDFHDLArgs = dd.paramss.view.flatten.exists {
                    case vd: ValDef => vd.dfcFuncTpeOpt.nonEmpty
                    case _          => false
                  }
                  if (hasDFHDLArgs)
                    report.error(
                      "A DFHDL definition (has at least one DFHDL argument) must also return a DFHDL value.",
                      dd.srcPos
                    )
                  super.transform(dd)
              end match
          case td: TypeDef =>
            withDFCIn(td):
              super.transform(td)
          case t: (UnApply | Closure) =>
            t.tpe.dfcFuncTpeOptRecur match
              case Some(tp) => super.transform(t).withType(tp)
              case None     => super.transform(t)
          case tt: TypeTree =>
            TypeTree(tt.tpe.dfcFuncTpeOptRecur.getOrElse(tt.tpe))
          case block @ Block(List(anonDef @ DefDef(_, List(List(param: ValDef)), _, _)), _: Closure)
              if param.tpt.tpe <:< metaContextTpe =>
            ownerMap += anonDef.symbol -> anonDef.symbol.owner
            replaceDFCMap += param.symbol -> updatedDFC
            transform(anonDef.rhs)
          case b: Bind =>
            b.body.tpe.dfcFuncTpeOptRecur match
              case Some(bTpe) =>
                val bSym = b.symbol
                val retSym = b.newDefSymbol(bSym.owner, bTpe)
                symMap += bSym -> retSym
                Bind(retSym, transform(b.body)).withSpan(b.span)
              case None => super.transform(b)
          case ident: Ident if replaceDFCMap.contains(ident.symbol) =>
            replaceDFCMap(ident.symbol)
          case ident: Ident if !ident.symbol.exists =>
            ident.tpe.dfcFuncTpeOptRecur match
              case Some(bTpe) => ident.withType(bTpe)
              case None       => ident
          case tree => super.transform(tree)
    .transform(tree)
    // debug("ONE")
    // debug(r1.show)
    val r2 = new TreeMap():
      override def transform(tree: Tree)(using Context): Tree =
        tree match
          case Apply(Select(Typed(x, att: AppliedTypeTree), applyFn), List(da))
              if applyFn == nme.apply && att.tpe.dealias.typeSymbol == contextFunctionSym =>
            transform(x).withSpan(tree.span)
          case Apply(Select(x, applyFn), List(da))
              if applyFn == nme.apply && x.tpe.typeSymbol == contextFunctionSym =>
            x match
              case Inlined(call, bindings, Typed(expr, tpt)) =>
                Inlined(call, bindings, transform(expr)).withSpan(tree.span)
              case Apply(_, args) if x.symbol.is(Method) && ownerMap.contains(x.symbol) =>
                Apply(
                  Apply(ref(ownerMap(x.symbol)), transformParams(args.asInstanceOf[ParamClause])),
                  List(da)
                ).withSpan(tree.span)
              case _ if !x.symbol.is(Method) && symMap.contains(x.symbol) =>
                ref(symMap(x.symbol)).withSpan(tree.span)
              case Block(List(anonDef: DefDef), closure: Closure) => super.transform(tree)
              case _                                              => transform(x)
          case Ident(_) if symMap.contains(tree.symbol) =>
            ref(symMap(tree.symbol)).withSpan(tree.span)
          case _ =>
            super.transform(tree)
    .transform(r1)
    // debug("TWO")
    // debug(r2.show)

    var checkOwnerUpdates: Boolean = true
    while (checkOwnerUpdates) do
      checkOwnerUpdates = false
      ownerMap.foreach { (f, t) =>
        ownerMap.get(t) match
          case Some(newTo) =>
            checkOwnerUpdates = true
            ownerMap += f -> newTo
          case _ => // do nothing
      }
    // debug("symMap")
    // debug(symMap)
    // debug("ownerMap")
    // debug(ownerMap)
    val (oldOwners, newOwners) = (ownerMap.keys.toList, ownerMap.values.toList)
    val typeMap: Type => Type = tp =>
      tp.dfcFuncTpeOpt match
        case Some(tp) =>
          debug(s"Bang: $tp")
          tp
        case _ => tp
    val r3 =
      new TreeTypeMap(oldOwners = oldOwners, newOwners = newOwners).apply(r2)
    // debug("THREE")
    // debug(r3.show)
    // check(r3)
    // checkRemainingContextDFVals(r3)
    r3
  end transformDFCFunc

  override def transformUnit(tree: Tree)(using Context): Tree =
    super.transformUnit(transformDFCFunc(tree))

//   private object HasDFCInstance:
//     @tailrec def unapply(tree: Tree)(using Context): Option[Type] =
//       tree match
//         case Select(clsTree @ New(id), _) => Some(clsTree.tpe)
//         case Apply(tree, _)               => unapply(tree)
//         case TypeApply(tree, _)           => unapply(tree)
//         case _                            => None

//   override def transformApply(tree: Apply)(using Context): Tree =
//     if (tree.tpe.isParameterless && tree.tpe <:< hasDFCTpe && !ignore.exists(i => i.sameTree(tree)))
//       tree match
//         case HasDFCInstance(clsTpe) if dfcClsStack.nonEmpty =>
//           debug(s"owner: ${dfcClsStack.head.symbol}")
//           debug(clsTpe)
//           val x = AnonClass(dfcClsStack.head.symbol, List(clsTpe), tree.symbol.coord)(_ => Nil)
//           debug(x.show)
//           tree
//         case _ => tree
//     else
//       tree

  override def prepareForUnit(tree: Tree)(using Context): Context =
    super.prepareForUnit(tree)
    ctx
end DFCOverridePhase
