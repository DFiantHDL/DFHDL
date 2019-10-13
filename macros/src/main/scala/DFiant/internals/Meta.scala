package DFiant.internals

import scala.annotation.tailrec
import scala.reflect.macros.blackbox

case class Meta(name : Meta.Name, position : Meta.Position, namePosition : Meta.Position)

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
  /////////////////////////////////////////////////////////

  /////////////////////////////////////////////////////////
  //Position
  /////////////////////////////////////////////////////////
  case class Position(file : String, line : Int, column : Int) {
    override def toString: String = s"$file:$line:$column"
  }
  /////////////////////////////////////////////////////////

  /////////////////////////////////////////////////////////
  //Name
  /////////////////////////////////////////////////////////
  case class Name(value : String) {
    override def toString: String = value
  }
  object Name {
    final val AnonStart : String = "dFt_"
    final val Separator : String = "_d_" //"ǂ"
    implicit def getString(name : Name) : String = name.value

    implicit def ev : Name = macro evMacro
    def evMacro(c: blackbox.Context): c.Expr[Name] = {
      import c.universe._
      val owner = getValidOwner(c)
      val name = getOwnerName(c)(owner)
      c.Expr[Meta.Name](q"""DFiant.internals.Meta.Name($name)""")
    }

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

  implicit def ev : Meta = macro evMacro
  def evMacro(c: blackbox.Context): c.Expr[Meta] = {
    import c.universe._
    val file = c.enclosingPosition.source.path
    val line = c.enclosingPosition.line
    val column = c.enclosingPosition.column
    val owner = getValidOwner(c)
    val name = getOwnerName(c)(owner)
    val nameFile = owner.pos.source.path
    val nameLine = owner.pos.line
    val nameColumn = owner.pos.column
    val anonymous = !(owner.isTerm || owner.isModuleClass || owner.isMethod) //not a val, lazy val, var, object or def
    val anonName : String = if (anonymous) s"${Name.AnonStart}anon" else name
    c.Expr[Meta](q"""${c.prefix}(DFiant.internals.Meta.Name($anonName), DFiant.internals.Meta.Position($file, $line, $column), DFiant.internals.Meta.Position($nameFile, $nameLine, $nameColumn))""")
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
