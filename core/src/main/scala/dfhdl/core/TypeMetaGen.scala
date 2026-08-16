package dfhdl.core
import dfhdl.compiler.ir
import scala.quoted.*

/** Builds a named DFType's declaration `ir.Meta` inside a derivation macro: name, enclosing Scala
  * package (namespace), declaration position, and doc comment, mirroring what the compiler plugin
  * captures for design classes and methods. Namespaces stop at the package level deliberately:
  * enclosing objects and classes are scoping, not namespacing, for the packages feature.
  */
private[core] object TypeMetaGen:
  def namespaceOf(using q: Quotes)(sym: q.reflect.Symbol): String =
    import quotes.reflect.*
    var owner = sym.owner
    while (!owner.isPackageDef) do owner = owner.owner
    val fullName = owner.fullName
    if (fullName.startsWith("<")) "" else fullName

  def apply(using q: Quotes)(sym: q.reflect.Symbol): Expr[ir.Meta] =
    import quotes.reflect.*
    val nameExpr = Expr(sym.name.toString)
    val namespaceExpr = Expr(namespaceOf(sym))
    val posExpr = sym.pos match
      case Some(pos) if scala.util.Try(pos.sourceFile.path).isSuccess =>
        '{
          dfhdl.internals.Position.fromAbsPath(
            ${ Expr(pos.sourceFile.path) },
            ${ Expr(pos.startLine + 1) },
            ${ Expr(pos.startColumn + 1) },
            ${ Expr(pos.endLine + 1) },
            ${ Expr(pos.endColumn + 1) }
          )
        }
      case _ => '{ dfhdl.internals.Position.unknown }
    val docExpr = Expr(sym.docstring)
    '{ ir.Meta(Some($nameExpr), $posExpr, $docExpr, Nil, $namespaceExpr) }
  end apply
end TypeMetaGen
