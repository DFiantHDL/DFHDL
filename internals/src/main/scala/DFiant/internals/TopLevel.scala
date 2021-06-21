package DFiant.internals
import scala.quoted.*
import annotation.tailrec

trait TopLevel
object TopLevel:
  inline given TopLevel = ${ evMacro }
  def evMacro(using Quotes): Expr[TopLevel] =
    import quotes.reflect.*
    @tailrec def getTopOwner(owner: Symbol): Symbol =
      if (owner.owner.isPackageDef) owner
      else getTopOwner(owner.owner)
    val topOwner = getTopOwner(Symbol.spliceOwner)
    val appSymbol = (TypeRepr.of[App]).typeSymbol
    val mainTpe = TypeRepr.of[main]
    val isTop =
      topOwner.isClassDef &&
        TypeRepr
          .of[Any]
          .memberType(topOwner)
          .baseClasses
          .contains(appSymbol) || //Top owner is the main object
        Symbol.spliceOwner.owner.annotations.exists(a => a.tpe <:< mainTpe) ||
        topOwner.name.toString == "$read" || //Top owner is REPL console
        topOwner.fullName.startsWith("ammonite.") //ammonite console

    if (isTop) '{ new TopLevel {} }
    else
      report.error("Not a top-level")
      '{ ??? }

trait AllowTopLevel:
  given TopLevel with {}
