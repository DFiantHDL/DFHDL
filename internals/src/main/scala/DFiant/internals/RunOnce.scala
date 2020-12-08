package DFiant.internals

import scala.reflect.macros.whitebox

sealed class RunOnce
object RunOnce {
  import collection.mutable
  private val cache = mutable.Map.empty[Any, Boolean]
  implicit def ev: RunOnce = macro evMacro
  def evMacro(c: whitebox.Context): c.Tree = {
    import c.universe._
    if (cache.getOrElse(c.enclosingPosition, false))
      c.abort(c.enclosingPosition, "Can only run once")
    else cache += (c.enclosingPosition -> true)
    q"new DFiant.internals.RunOnce"
  }
}
