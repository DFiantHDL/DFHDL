package DFiant.internals

import DFiant.internals.Meta.Name.Anonymous

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
  def anonymize : Meta = copy(name = name.anonymize)
  def isAnonymous : Boolean = name.isAnonymous
  def isNameForced : Boolean = name.isNameForced
  def setName(name : String) : Meta = copy(name = this.name.set(name))
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
  final case class Position(file : String, line : Int, column : Int) {
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
  final case class Name(value : String, anonymous : Name.Anonymous) {
    def prefix : String = anonymous match {
      case Anonymous.On => Name.AnonStart
      case _ => ""
    }
    def anonymize : Name = copy(anonymous = Name.Anonymous.On)
    def isAnonymous : Boolean = anonymous match {
      case Anonymous.On => true
      case _ => false
    }
    def isNameForced : Boolean = anonymous match {
      case Anonymous.ForceOff => true
      case _ => false
    }
    def set(value : String) : Name = Name(value, Name.Anonymous.ForceOff)
    override def toString: String = s"$prefix$value"
  }
  object Name {
    sealed trait Anonymous extends Product with Serializable
    object Anonymous {
      case object Off extends Anonymous
      case object On extends Anonymous
      case object ForceOff extends Anonymous
    }
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
  }
  /////////////////////////////////////////////////////////

  final case class SymbolOf[T](value: String)
  object SymbolOf {
    implicit def ev[T]: SymbolOf[T] = macro evMacro[T]
    def evMacro[T](c: blackbox.Context)(implicit t : c.WeakTypeTag[T]): c.Expr[SymbolOf[T]] = {
      import c.universe._
      val sym = symbolOf[T]
      val name = sym.fullName
      c.Expr[SymbolOf[T]](q"""${c.prefix}($name)""")
    }
  }

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
    val enclosingOwnerName = c.internal.enclosingOwner.name.toString
    val anonymous =
      if (enclosingOwnerName.contains("<local") || enclosingOwnerName == "$anonfun") q"DFiant.internals.Meta.Name.Anonymous.On"
      else q"DFiant.internals.Meta.Name.Anonymous.Off"
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
