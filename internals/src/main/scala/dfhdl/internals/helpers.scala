package dfhdl.internals
import scala.quoted.*
import scala.annotation.tailrec
import scala.collection.immutable.{ListMap, ListSet}
import scala.collection.mutable
import scala.util.matching.Regex
extension [T](t: T)
  def debugPrint: T =
    println(t)
    t
def clog2(int: Int): Int = 32 - Integer.numberOfLeadingZeros(int - 1)

extension (t: Any) def hashString: String = t.hashCode().toHexString

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
    val cache = mutable.Map.empty[(T, T), Boolean]
    final class Unique(val value: T):
      override def equals(that: Any): Boolean =
        val thatUniqueValue = that.asInstanceOf[Unique].value
        cache.getOrElseUpdate(
          (value, thatUniqueValue),
          cache.getOrElseUpdate(
            (thatUniqueValue, value),
            customEq(this.value, thatUniqueValue)
          )
        )
      override def hashCode(): Int = customHash(value)
    lhs.groupBy(new Unique(_)).values.map(_.toList)
end extension

extension (using quotes: Quotes)(tpe: quotes.reflect.TypeRepr)
  def asTypeOf[T <: AnyKind]: Type[T] =
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
  def funcName: String =
    import quotes.reflect.*
    sc.asTerm.underlying match
      case TypeApply(Select(Apply(func, _), _), _) => func.symbol.name
      case _ => report.errorAndAbort("Could not detect interpolation function name.")
  def parts: Seq[Expr[String]] =
    import quotes.reflect.*
    val interp = sc.asTerm.underlying match
      case TypeApply(Select(Apply(func, List(interp)), _), _) => interp
      case interp                                             => interp
    val '{ StringContext(${ Varargs(partsExprs) }*) } = interp.asExpr: @unchecked
    partsExprs
  def scPartsWithArgs(args: Expr[Seq[Any]]): quotes.reflect.Term =
    import quotes.reflect.*
    val Varargs(argsExprs) = args: @unchecked
    sc.parts.scPartsWithArgs(argsExprs)
end extension

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

type <:![T <: UB, UB] = T & UB

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
  object Success extends AssertGiven[Any, String]
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

    if (recur(TypeRepr.of[G])) '{ Success.asInstanceOf[AssertGiven[G, M]] }
    else
      val ConstantType(StringConstant(msg)) = TypeRepr.of[M].dealias: @unchecked
      '{ compiletime.error(${ Expr(msg) }) }
  end macroImpl
end AssertGiven

trait OptionalGiven[T]:
  val value: Option[T]
  def get: T = value.get
  def getOrElse(default: => T): T = value.getOrElse(default)
protected trait OptionalGivenLP:
  given fromNone[T]: OptionalGiven[T] with
    val value: Option[T] = None
object OptionalGiven extends OptionalGivenLP:
  given fromValue[T](using t: T): OptionalGiven[T] with
    val value: Option[T] = Some(t)

trait IsGiven[T]:
  type Out <: Boolean
  val value: Out
protected trait IsGivenLP:
  given not[T]: IsGiven[T] with
    type Out = false
    val value: false = false
object IsGiven extends IsGivenLP:
  given is[T](using t: T): IsGiven[T] with
    type Out = true
    val value: true = true

//from Map[K,V] to Map[V,Set[K]], traverse the input only once
//From: https://stackoverflow.com/a/51356499/3845175
extension [K, V](m: Map[K, V])
  def invert: Map[V, Set[K]] =
    m.foldLeft(Map.empty[V, Set[K]]) { case (acc, (k, v)) =>
      acc + (v -> (acc.getOrElse(v, Set()) + k))
    }

//from ListMap[K,V] to ListMap[V,ListSet[K]], traverse the input only once
//From: https://stackoverflow.com/a/51356499/3845175
extension [K, V](m: ListMap[K, V])
  def invert: ListMap[V, ListSet[K]] =
    m.foldLeft(ListMap.empty[V, ListSet[K]]) { case (acc, (k, v)) =>
      acc + (v -> (acc.getOrElse(v, ListSet()) + k))
    }

extension [T](list: List[T])
  def emptyOr(f: List[T] => String): String =
    if (list.isEmpty) "" else f(list)

extension (str: String)
  def emptyOr(f: String => String): String =
    if (str.isEmpty) str else f(str)
  // We translate the windows `\` to unix `/`
  def forceWindowsToLinuxPath: String = str.replaceAll("""\\""", "/")
  def simplePattenToRegex: Regex =
    val updatedPattern =
      "^" + str
        .replace(".", "\\.") // Escape dots to match literal dots
        .replace("*", ".*") // Replace * with .*
        .replace("?", ".") // Replace ? with .
        + "$"
    updatedPattern.r
end extension
extension [T](seq: Iterable[T])
  def groupByOrdered[P](f: T => P): List[(P, List[T])] =
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
    accumulator(seq, f, Nil).view.map(e => (e._1, e._2.toList)).toList

lazy val getShellCommand: Option[String] =
  import scala.io.Source
  import scala.util.{Try, Success, Failure}
  import sys.process.*
  val osName = System.getProperty("os.name").toLowerCase
  val pid = Try(java.lang.management.ManagementFactory.getRuntimeMXBean.getName.split("@")(0))

  pid match
    case Success(id) =>
      val command =
        if osName.contains("linux") then
          Try(Source.fromFile(s"/proc/$id/cmdline").mkString.replace("\u0000", " "))
        else if osName.contains("mac") then Try(s"ps -p $id -o command=".!!)
        else if osName.contains("win") then
          Try(
            s"wmic process where processid=$id get commandline".!!
              .split("\n").drop(1).mkString
          )
        else Failure(new UnsupportedOperationException("Unsupported OS"))

      command match
        case Success(cmd) => Some(cmd.trim)
        case Failure(_)   => None

    case Failure(_) => None
  end match
end getShellCommand

lazy val sbtShellIsRunning: Boolean =
  getShellCommand.exists(cmd => cmd.endsWith("xsbt.boot.Boot") || cmd.endsWith("sbt-launch.jar"))

lazy val sbtTestIsRunning: Boolean =
  getShellCommand.exists(cmd =>
    cmd.endsWith("xsbt.boot.Boot test") || cmd.endsWith("sbt-launch.jar test")
  )

lazy val sbtIsRunning: Boolean =
  getShellCommand.exists(cmd => cmd.contains("xsbt.boot.Boot") || cmd.contains("sbt-launch.jar"))

lazy val scala_cliIsRunning: Boolean = getShellCommand.exists(_.contains(".scala-build"))

// detecting if running in Scastie by checking the PWD
lazy val scastieIsRunning: Boolean =
  System.getProperty("user.dir").startsWith("/tmp/scastie")

// detecting if running from Metals
lazy val metalsIsRunning: Boolean =
  getShellCommand.exists(cmd =>
    cmd.contains("""\.metals\.tmp""") || cmd.contains("""/.metals/.tmp""")
  )

// detecting if running from IntelliJ IDEA
lazy val ideaIsRunning: Boolean =
  getShellCommand.exists(cmd => cmd.contains("idea_rt.jar="))

def getRelativePath(absolutePathStr: String): String =
  import java.nio.file.Paths
  val absolutePath = Paths.get(absolutePathStr).toAbsolutePath()
  val currentDir = Paths.get("").toAbsolutePath()
  currentDir.relativize(absolutePath).toString

inline def compiletimeErrorPos(
    inline msg: String,
    inline start: Int,
    inline end: Int
): Nothing = ${
  compiletimeErrorPosMacro('msg, 'start, 'end)
}
def compiletimeErrorPosMacro(msg: Expr[String], start: Expr[Int], end: Expr[Int])(using
    Quotes
): Expr[Nothing] =
  import quotes.reflect.*
  val updatedPos =
    Position(Position.ofMacroExpansion.sourceFile, Expr.unapply(start).get, Expr.unapply(end).get)
  report.errorAndAbort(Expr.unapply(msg).get, updatedPos)

extension (using quotes: Quotes)(term: quotes.reflect.Term)
  def compiletimeErrorPosExpr(msg: String): Expr[Nothing] =
    val msgExpr = Expr(msg)
    val start = Expr(term.pos.start)
    val end = Expr(term.pos.end)
    '{ compiletimeErrorPos($msgExpr, $start, $end) }

// trait CompiletimeErrorPos[M <: String, S <: Int, E <: Int]
// object CompiletimeErrorPos:
//   inline given [M <: String, S <: Int, E <: Int]: CompiletimeErrorPos[M, S, E] =
//     ${ ofMacro[M, S, E] }

//   def ofMacro[M <: String: Type, S <: Int: Type, E <: Int: Type](using
//       Quotes
//   ): Expr[CompiletimeErrorPos[M, S, E]] =
//     import quotes.reflect.*
//     val ConstantType(StringConstant(msg)) = TypeRepr.of[M]: @unchecked
//     val ConstantType(IntConstant(start)) = TypeRepr.of[S]: @unchecked
//     val ConstantType(IntConstant(end)) = TypeRepr.of[E]: @unchecked
//     val updatedPos = Position(Position.ofMacroExpansion.sourceFile, start, end)
//     report.errorAndAbort(msg, updatedPos)

// object CompiletimeErrorCache:
//   var cachedError: String = ""
//   transparent inline def cache(
//       inline msg: String
//   ): Unit = ${ cacheMacro('msg) }
//   private def cacheMacro(msg: Expr[String])(using Quotes): Expr[Unit] =
//     import quotes.reflect.*
//     val msgStr = Expr.unapply(msg).get
//     cachedError = msgStr
//     println("caching")
//     '{}
//   transparent inline def clearCache(): Unit = ${ clearCacheMacro() }
//   private def clearCacheMacro()(using Quotes): Expr[Unit] =
//     import quotes.reflect.*
//     println(s"clearing cache")
//     cachedError = ""
//     '{}
//   transparent inline def reportCache(): Unit = ${ reportCacheMacro() }
//   private def reportCacheMacro()(using Quotes): Expr[Unit] =
//     import quotes.reflect.*
//     println(s"reporting cache: $cachedError")
//     val msgExpr = Expr(cachedError)
//     if (cachedError.nonEmpty)
//       '{ compiletime.error($msgExpr) }
//     else
//       '{}
//   inline def report(
//       inline msg: String
//   ): Nothing = ${ reportMacro('msg) }
//   private def reportMacro(msg: Expr[String])(using Quotes): Expr[Nothing] =
//     import quotes.reflect.*
//     val msgStr = Expr.unapply(msg).get
//     cachedError = msgStr
//     println("caching")
//     quotes.reflect.report.errorAndAbort(msgStr)
// end CompiletimeErrorCache
