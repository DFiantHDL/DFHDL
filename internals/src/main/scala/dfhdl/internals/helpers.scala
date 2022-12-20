package dfhdl.internals
import scala.quoted.*
import scala.annotation.tailrec
extension [T](t: T)
  def debugPrint: T =
    println(t)
    t

transparent inline def showTree[T](inline arg: T): Unit = ${
  showTreeMacro[T]('arg)
}

def errorExpr(msg: String)(using Quotes): Expr[Nothing] =
  '{ compiletime.error(${ Expr(msg) }) }

def showTreeMacro[T](arg: Expr[T])(using Quotes, Type[T]): Expr[Unit] =
  import quotes.reflect.*
  val Inlined(_, _, term) = arg.asTerm: @unchecked
  println(term.show)
  println(term)
  println(TypeRepr.of[T].show)
  println(term.tpe.show)
  '{}

extension [T](t: Iterable[T])(using CanEqual[T, T])
  def mkStringBrackets: String = t.mkString("(", ", ", ")")
  def allElementsAreEqual: Boolean = t.forall(_ == t.head)

extension [T](lhs: Iterable[T])
  def coalesce(rhs: Iterable[T]): Iterable[T] =
    Seq(lhs, rhs)
      .flatMap(_.zipWithIndex)
      .sortBy(_._2)
      .map(_._1)
  def groupByCompare(customEq: (T, T) => Boolean, customHash: T => Int): Iterable[List[T]] =
    final class Unique(val value: T):
      override def equals(that: Any): Boolean =
        customEq(this.value, that.asInstanceOf[Unique].value)
      override def hashCode(): Int = customHash(value)
    lhs.groupBy(new Unique(_)).view.map((u, i) => i.toList)
end extension

extension (using quotes: Quotes)(tpe: quotes.reflect.TypeRepr)
  def asTypeOf[T]: Type[T] =
    import quotes.reflect.*
    tpe.asType.asInstanceOf[Type[T]]
  def asTypeTree: quotes.reflect.TypeTree =
    import quotes.reflect.*
    tpe.asType match
      case '[t] =>
        TypeTree.of[t]
  // gets the class type from its companion object type
  def getCompanionClassTpe: quotes.reflect.TypeRepr =
    import quotes.reflect.*
    val compPrefix = tpe match
      case TermRef(pre, _) => pre
      case _               => report.errorAndAbort("Case class companion must be a term ref")
    val clsSym = tpe.typeSymbol.companionClass
    if !clsSym.paramSymss.forall(_.headOption.forall(_.isTerm)) then
      report.errorAndAbort("Case class with type parameters are not supported")
    compPrefix.select(clsSym)

end extension

extension (using quotes: Quotes)(lhs: quotes.reflect.TypeRepr)
  def getTupleArgs: List[quotes.reflect.TypeRepr] =
    import quotes.reflect.*
    @tailrec def recur(tpl: TypeRepr, args: List[TypeRepr]): List[TypeRepr] =
      tpl.asTypeOf[Any] match
        case '[head *: tail] =>
          recur(TypeRepr.of[tail], TypeRepr.of[head] :: args)
        case '[EmptyTuple] => args.reverse
        case '[t] =>
          report.errorAndAbort(s"Expecting a tuple, but found ${Type.show[t]}")
    if (lhs.show == "null")
      report.errorAndAbort(s"Expecting a tuple, but found null")
    else
      recur(lhs, Nil)
end extension

trait PrintType[T]
object PrintType:
  inline given [T]: PrintType[T] = ${ macroImpl[T] }
  def macroImpl[T](using Quotes, Type[T]): Expr[PrintType[T]] =
    import quotes.reflect.*
    println(Type.show[T])
    '{ new PrintType[T] {} }

object Error:
  transparent inline def call[T <: NonEmptyTuple]: Nothing = ${ macroImpl[T] }
  def macroImpl[T <: NonEmptyTuple](using Quotes, Type[T]): Expr[Nothing] =
    import quotes.reflect.*
    val msg =
      TypeRepr
        .of[T]
        .getTupleArgs
        .map(_.dealias)
        .map {
          case ConstantType(StringConstant(msg)) => msg
          case t                                 => t.show
        }
        .mkString
    '{ compiletime.error(${ Expr(msg) }) }
end Error

extension (using quotes: Quotes)(partsExprs: Seq[Expr[Any]])
  def scPartsWithArgs(argsExprs: Seq[Expr[Any]]): quotes.reflect.Term =
    import quotes.reflect.*
    val fullTermParts = partsExprs.coalesce(argsExprs).map(_.asTerm)
    fullTermParts.reduce[Term] {
      case (Literal(StringConstant(l)), Literal(StringConstant(r))) =>
        Literal(StringConstant(l + r))
      case (l, r) =>
        '{ ${ l.asExpr }.toString + ${ r.asExpr }.toString }.asTerm
    }

extension (using quotes: Quotes)(sc: Expr[StringContext])
  def scPartsWithArgs(args: Expr[Seq[Any]]): quotes.reflect.Term =
    import quotes.reflect.*
    val argsExprs = args match
      case Varargs(argsExprs) => argsExprs
    val '{ StringContext.apply($parts*) } = sc: @unchecked
    val partsExprs = parts match
      case Varargs(argsExprs) => argsExprs
    partsExprs.scPartsWithArgs(argsExprs)

extension [P <: Tuple](using quotes: Quotes, p: Type[P])(partsTpl: Expr[P])
  def scPartsWithArgs(args: Expr[Seq[Any]]): quotes.reflect.Term =
    import quotes.reflect.*
    val argsExprs = args match
      case Varargs(argsExprs) => argsExprs
    val partsExprs = SIParts.tupleToExprs(partsTpl)
    partsExprs.scPartsWithArgs(argsExprs)

inline implicit def fromValueOf[T](v: ValueOf[T]): T = v.value

trait ValueOfTuple[T <: NonEmptyTuple]:
  val value: T
object ValueOfTuple:
  inline given [T <: NonEmptyTuple]: ValueOfTuple[T] = ${ givenMacro[T] }
  def givenMacro[T <: NonEmptyTuple: Type](using Quotes): Expr[ValueOfTuple[T]] =
    import quotes.reflect.*
    // TODO: to generically handle this we need to support tuple of tuples,
    // but currently where this is used (dimension tuple of vector) it won't happen.
    def recur(tpe: TypeRepr): Term =
      tpe.asTypeOf[Any] match
        case '[NonEmptyTuple] =>
          val AppliedType(fun, args) = tpe: @unchecked
          Expr.ofTupleFromSeq(args.map(a => recur(a).asExprOf[Any])).asTerm
        case '[x] =>
          '{ compiletime.summonInline[ValueOf[x]].value }.asTerm
    val valueExpr = recur(TypeRepr.of[T]).asExprOf[T]
    '{
      new ValueOfTuple[T]:
        val value: T = $valueExpr
    }
  end givenMacro
end ValueOfTuple

type <:![T <: UB, UB] <: UB = T match
  case UB => T

//evidence of class T which has no arguments and no type arguments
trait ClassEv[T]:
  val value: T
object ClassEv:
  inline given [T]: ClassEv[T] = ${ macroImpl[T] }
  def macroImpl[T](using Quotes, Type[T]): Expr[ClassEv[T]] =
    import quotes.reflect.*
    val tpe = TypeRepr.of[T]
    val sym = tpe.typeSymbol
    if (sym.flags.is(Flags.Abstract)) report.errorAndAbort(s"Class `${sym.name}` is abstract.")
    val valueExpr = New(tpe.asTypeTree)
      .select(sym.primaryConstructor)
      .appliedToNone
      .asExprOf[T]
    '{
      new ClassEv[T]:
        val value: T = $valueExpr
    }
end ClassEv

// gets the case class from a companion object reference
trait CaseClass[Companion <: AnyRef, UB <: Product]:
  type CC <: UB

object CaseClass:
  type Aux[Comp <: AnyRef, UB <: Product, CC0 <: UB] = CaseClass[Comp, UB] { type CC = CC0 }
  transparent inline given [Comp <: AnyRef, UB <: Product]: CaseClass[Comp, UB] = ${
    macroImpl[Comp, UB]
  }
  def macroImpl[Comp <: AnyRef, UB <: Product](using
      Quotes,
      Type[Comp],
      Type[UB]
  ): Expr[CaseClass[Comp, UB]] =
    import quotes.reflect.*
    val clsTpe = TypeRepr.of[Comp].getCompanionClassTpe
    clsTpe.asType match
      case '[t & UB] =>
        type Case = t & UB
        '{
          new CaseClass[Comp, UB]:
            type CC = Case
        }
      case _ =>
        val msg =
          s"Type `${clsTpe.show}` is not a subtype of `${Type.show[UB]}`."
        '{
          compiletime.error(${ Expr(msg) })
          new CaseClass[Comp, UB]:
            type CC = UB
        }
    end match
  end macroImpl
end CaseClass

trait AssertGiven[G, M <: String]
object AssertGiven:
  transparent inline given [G, M <: String]: AssertGiven[G, M] =
    ${ macroImpl[G, M] }
  def macroImpl[G, M <: String](using
      Quotes,
      Type[G],
      Type[M]
  ): Expr[AssertGiven[G, M]] =
    import quotes.reflect.*
    def recur(tpe: TypeRepr): Boolean =
      tpe match
        case OrType(l, r) =>
          if (recur(l)) true
          else recur(r)
        case _ =>
          tpe.asTypeOf[Any] match
            case '[x] =>
              Expr.summon[x].nonEmpty

    if (recur(TypeRepr.of[G])) '{ new AssertGiven[G, M] {} }
    else
      val ConstantType(StringConstant(msg)) = TypeRepr.of[M].dealias: @unchecked
      '{ compiletime.error(${ Expr(msg) }) }
  end macroImpl
end AssertGiven

//from Map[K,V] to Map[V,Set[K]], traverse the input only once
//From: https://stackoverflow.com/a/51356499/3845175
extension [K, V](m: Map[K, V])
  def invert: Map[V, Set[K]] =
    m.foldLeft(Map.empty[V, Set[K]]) { case (acc, (k, v)) =>
      acc + (v -> (acc.getOrElse(v, Set()) + k))
    }

extension [T](iter: Iterable[T])
  def emptyOr(f: Iterable[T] => String): String =
    if (iter.isEmpty) "" else f(iter)

extension (str: String)
  def emptyOr(f: String => String): String =
    if (str.isEmpty) str else f(str)

extension [T](seq: Iterable[T])
  def groupByOrdered[P](f: T => P): Seq[(P, Iterable[T])] =
    @tailrec
    def accumulator(
        seq: Iterable[T],
        f: T => P,
        res: List[(P, Iterable[T])]
    ): Seq[(P, Iterable[T])] = seq.headOption match
      case None => res.reverse
      case Some(h) =>
        val key = f(h)
        val subseq = seq.takeWhile(f(_) equals key)
        accumulator(seq.drop(subseq.size), f, (key -> subseq) :: res)
    accumulator(seq, f, Nil)
