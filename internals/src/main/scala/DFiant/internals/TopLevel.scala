package DFiant.internals

import scala.reflect.macros.blackbox

sealed class TopLevel
object TopLevel {
  implicit def ev : TopLevel = macro evMacro
  def evMacro(c: blackbox.Context) : c.Tree = {
    import c.universe._
    val owner = c.internal.enclosingOwner
    if (
      owner.owner.name.toString == "$iw" || //in Scala REPL
      (!owner.isConstructor && !owner.isTerm) ||
      owner.isConstructor && owner.owner.name.toString == "$anon" ||
      owner.owner.isModuleClass) q"new DFiant.internals.TopLevel"
    else {
      c.abort(c.enclosingPosition, "Not a top-level")
    }
  }
}
