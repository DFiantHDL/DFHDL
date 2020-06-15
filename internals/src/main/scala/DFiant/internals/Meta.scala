package DFiant.internals

import scala.annotation.tailrec
import scala.reflect.macros.blackbox
sealed trait LateConstructionConfig {
  def apply(value : Boolean) : Boolean
}
object LateConstructionConfig {
  implicit case object Auto extends LateConstructionConfig {
    def apply(value: Boolean): Boolean = value
  }
  case class Force(forcedValue : Boolean) extends LateConstructionConfig {
    def apply(value: Boolean): Boolean = forcedValue
  }
}
final case class Meta(name : Meta.Name, position : Meta.Position, namePosition : Meta.Position, lateConstruction : Boolean) {
  def anonymize : Meta = copy(name = name.copy(anonymous = true))
  def setName(name : String) : Meta = copy(name = this.name.copy(name, anonymous = false))
}

object Meta {
  /////////////////////////////////////////////////////////
  //Helper defs
  /////////////////////////////////////////////////////////
  private def getOwnerName(c : blackbox.Context)(owner : c.Symbol) : String = owner.name.decodedName.toString.trim
  @tailrec private def _getValidOwner(c : blackbox.Context)(owner : c.Symbol) : c.Symbol = {
    val name = getOwnerName(c)(owner)
    if (name == "<init>" || (name.startsWith("<local ") && name.endsWith(">")) || name.contains("$")) _getValidOwner(c)(owner.owner)
    else owner
  }
  private def getValidOwner(c : blackbox.Context) = _getValidOwner(c)(c.internal.enclosingOwner)

  @tailrec private def _isOwnedByAnonymousClass(c : blackbox.Context)(owner : c.Symbol) : Boolean = {
    if (owner.isClass && owner.name.toString != "$anonfun") owner.name.toString.contains("$")
    else if (owner.isConstructor) _isOwnedByAnonymousClass(c)(owner.owner.owner) //jumping above the current class that belongs to this constructor
    else if (owner.isPackage) false
    else _isOwnedByAnonymousClass(c)(owner.owner)
  }
  private def isOwnedByAnonymousClass(c : blackbox.Context) = _isOwnedByAnonymousClass(c)(c.internal.enclosingOwner)
  /////////////////////////////////////////////////////////

  /////////////////////////////////////////////////////////
  //Position
  /////////////////////////////////////////////////////////
  case class Position(file : String, line : Int, column : Int) {
    def > (that : Position) : Boolean = {
      assert(file == that.file, "Can only compare positions within the same file")
      line > that.line || (line == that.line && column > that.column)
    }
    def >= (that : Position) : Boolean = (this == that) || (this > that)
    def < (that : Position) : Boolean = !(this >= that)
    def <= (that : Position) : Boolean = !(this > that)
    override def toString: String = s"$file:$line:$column"
  }
  /////////////////////////////////////////////////////////

  /////////////////////////////////////////////////////////
  //Name
  /////////////////////////////////////////////////////////
  case class Name(value : String, anonymous : Boolean) {
    def prefix : String = if (anonymous) Name.AnonStart else ""
    override def toString: String = s"$prefix$value"
  }
  object Name {
    final val AnonStart : String = "dFt_"
    final val Separator : String = "_d_" //"ǂ"
    implicit def getString(name : Name) : String = name.toString
//
//    implicit def ev : Name = macro evMacro
//    def evMacro(c: blackbox.Context): c.Expr[Name] = {
//      import c.universe._
//      val owner = getValidOwner(c)
//      val name = getOwnerName(c)(owner)
//      c.Expr[Meta.Name](q"""DFiant.internals.Meta.Name($name)""")
//    }

    case class OfType[T](value: String)
    object OfType {
      implicit def ev[T]: OfType[T] = macro evMacro[T]
      def evMacro[T](c: blackbox.Context)(implicit t : c.WeakTypeTag[T]): c.Expr[OfType[T]] = {
        import c.universe._
        val sym = weakTypeOf[T]
        val name = sym.toString
        c.Expr[OfType[T]](q"""${c.prefix}($name)""")
      }
    }

    case class OfSymbol[T](value: String)
    object OfSymbol {
      implicit def ev[T]: OfSymbol[T] = macro evMacro[T]
      def evMacro[T](c: blackbox.Context)(implicit t : c.WeakTypeTag[T]): c.Expr[OfSymbol[T]] = {
        import c.universe._
        val sym = symbolOf[T]
        val name = sym.name.toString
        c.Expr[OfSymbol[T]](q"""${c.prefix}($name)""")
      }
    }

  }
  /////////////////////////////////////////////////////////

  implicit def ev(implicit lateConstructionConfig: LateConstructionConfig) : Meta = macro evMacro
  def evMacro(c: blackbox.Context)(lateConstructionConfig : c.Tree): c.Expr[Meta] = {
    import c.universe._
    val file = c.enclosingPosition.source.path
    val line = c.enclosingPosition.line
    val column = c.enclosingPosition.column
    val owner = getValidOwner(c)
    val name = getOwnerName(c)(owner)
    val lateConstruction = isOwnedByAnonymousClass(c)
    val nameFile = owner.pos.source.path
    val nameLine = owner.pos.line
    val nameColumn = owner.pos.column
    val anonymous = c.internal.enclosingOwner.name.toString.contains("<local")//!(owner.isTerm || owner.isModuleClass || owner.isMethod) //not a val, lazy val, var, object or def
    c.Expr[Meta](q"""${c.prefix}(DFiant.internals.Meta.Name($name, $anonymous), DFiant.internals.Meta.Position($file, $line, $column), DFiant.internals.Meta.Position($nameFile, $nameLine, $nameColumn), $lateConstructionConfig($lateConstruction))""")
  }

  import singleton.ops._
  case class IsVar()
  object IsVar {
    implicit def ev : IsVar = macro evMacro
    def evMacro(c: blackbox.Context): c.Expr[IsVar] = {
      import c.universe._
      val owner = getValidOwner(c)
      if (owner.isTerm && owner.asTerm.isVar) c.Expr[IsVar](q"""${c.prefix}()""")
      else c.abort(c.enclosingPosition, "var is not allowed")
    }
  }

  type ForceNotVar[Sym] = RequireMsgSym[![ImplicitFound[IsVar]], VarDFTypes.Msg, Sym]
  final object VarDFTypes extends ErrorMsg (
    "Don't use `var` with dataflow values/variables.",
    "dont-use-var-with-dataflow-valuesvariables"
  )

}
