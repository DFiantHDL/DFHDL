package DFiant.internals

import scala.reflect.macros.blackbox
import scala.annotation.tailrec

sealed class TopLevel
object TopLevel {
  implicit def ev : TopLevel = macro evMacro
  def evMacro(c: blackbox.Context) : c.Tree = {
    import c.universe._
    @tailrec def getTopOwner(owner : Symbol) : Symbol = {
      if (owner.owner.isPackage) owner
      else getTopOwner(owner.owner)
    }
    val topOwner = getTopOwner(c.internal.enclosingOwner)
    val isTop =
      topOwner.isClass && topOwner.asClass.baseClasses.contains(symbolOf[App]) || //Top owner is the main object
        topOwner.name.toString == "$read" || //Top owner is REPL console
        topOwner.name.toString == "cmd1" //ammonite console

    if (isTop) q"new DFiant.internals.TopLevel"
    else c.abort(c.enclosingPosition, "Not a top-level")
  }
}
