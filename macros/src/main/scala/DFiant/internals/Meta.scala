package DFiant.internals

import scala.annotation.tailrec
import scala.reflect.macros.blackbox

case class Meta2(name : Meta2.Name, position : Meta2.Position, namePosition : Meta2.Position)

object Meta2 {
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

    implicit def ev : Name = macro evMacro
    def evMacro(c: blackbox.Context): c.Expr[Name] = {
      import c.universe._
      val owner = getValidOwner(c)
      val name = getOwnerName(c)(owner)
      c.Expr[Meta2.Name](q"""Meta2.Name($name)""")
    }
  }
  /////////////////////////////////////////////////////////

  implicit def ev : Meta2 = macro evMacro
  def evMacro(c: blackbox.Context): c.Expr[Meta2] = {
    import c.universe._
    val file = c.enclosingPosition.source.path
    val line = c.enclosingPosition.line
    val column = c.enclosingPosition.column
    val owner = getValidOwner(c)
    val name = getOwnerName(c)(owner)
    val anonymous = !(owner.isTerm || owner.isModuleClass || owner.isMethod) //not a val, lazy val, var, object or def
    val anonName : String = if (anonymous) s"${Name.AnonStart}anon" else name
    c.Expr[Meta2](q"""Meta2(Meta2.Name($anonName), Meta2.Position($file, $line, $column), Meta2.Position($file, $line, $column))""")
  }

  import singleton.ops._
  case class IsVar()
  object IsVar {
    implicit def ev : IsVar = macro evMacro
    def evMacro(c: blackbox.Context): c.Expr[IsVar] = {
      import c.universe._
      val owner = getValidOwner(c)
      if (owner.isTerm && owner.asTerm.isVar) c.Expr[IsVar](q"""Meta2.IsVar()""")
      else c.abort(c.enclosingPosition, VarDFTypes.msg)
    }
  }

  type ForceNotVar[Sym] = RequireMsgSym[![ImplicitFound[IsVar]], VarDFTypes.Msg, Sym]
  final object VarDFTypes extends ErrorMsg (
    "Don't use `var` with dataflow values/variables.",
    "dont-use-var-with-dataflow-valuesvariables"
  ) {final val msg = getMsg}

}
