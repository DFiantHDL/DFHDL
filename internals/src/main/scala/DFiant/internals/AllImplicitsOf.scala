package DFiant.internals
import scala.language.experimental.macros
import scala.reflect.internal.util
import scala.reflect.macros.{blackbox, contexts}

trait AllImplicitsOf[T] {
  val list : List[T]
}

object AllImplicitsOf {
  implicit def get[T]: AllImplicitsOf[T] = macro getMacro[T]

  def getMacro[T : c.WeakTypeTag](c: blackbox.Context): c.Tree = {
    import c.universe._

    val context = c.asInstanceOf[contexts.Context]
    val global: context.universe.type = context.universe
    val analyzer: global.analyzer.type = global.analyzer
    val callsiteContext = context.callsiteTyper.context

    val tpe = weakTypeOf[T]

    val search = new analyzer.ImplicitSearch(
      tree = EmptyTree.asInstanceOf[global.Tree],
      pt = tpe.asInstanceOf[global.Type],
      isView = false,
      context0 = callsiteContext.makeImplicit(reportAmbiguousErrors = false),
      pos0 = c.enclosingPosition.asInstanceOf[util.Position]
    )

    val list = search.allImplicits.map(_.tree.asInstanceOf[Tree])
    q"""new DFiant.internals.AllImplicitsOf[$tpe]{
      val list : List[$tpe] = $list
    }"""
  }
}
