package dfhdl.internals
import scala.quoted.*
import annotation.tailrec

trait TopLevel
object TopLevel:
  inline given TopLevel = ${ evMacro }
  def evMacro(using Quotes): Expr[TopLevel] =
    import quotes.reflect.*
    @tailrec def getTopOwner(owner: Symbol): Symbol =
      // stop at method as owner, unless the method is anonymous
      if (
        owner.flags.is(Flags.Method) &&
        !owner.name.startsWith("$") && owner.name != "<init>" ||
        owner.owner.isPackageDef
      )
        owner
      else getTopOwner(owner.owner)
    val topOwner = getTopOwner(Symbol.spliceOwner)
    val appSymbol = (TypeRepr.of[App]).typeSymbol
    val mainTpe = TypeRepr.of[main]
    val baseClasses = topOwner.typeRef.baseClasses
    val isTop =
      topOwner.name.contains("$package$") ||
        topOwner.isClassDef &&
        baseClasses.contains(appSymbol) || // Top owner is the main object
        Symbol.spliceOwner.owner.annotations.exists(a => a.tpe <:< mainTpe) ||
        // a DFHDL enum
        baseClasses.contains(Symbol.requiredClass("dfhdl.core.DFEncoding")) ||
        // an DFHDL opaque
        baseClasses.contains(Symbol.requiredClass("dfhdl.core.DFOpaque.Abstract")) ||
        topOwner.name.endsWith("$_") || // scala-cli top
        topOwner.name == "$read" || // Top owner is REPL console
        topOwner.fullName.startsWith("ammonite.") // ammonite console

    if (isTop) '{ new TopLevel {} }
    else
      report.errorAndAbort("Not a top-level")
      '{ ??? }
  end evMacro
end TopLevel

trait AllowTopLevel:
  given TopLevel with {}
