package DFiant.internals
import scala.reflect.macros.whitebox
import singleton.ops._

import scala.annotation.implicitNotFound

trait ExactType[UniqueTag] {
  type Summon[-From, V, +To] = ExactType.SummonGen[UniqueTag, From, V, To]
  object Summon {
    type Aux[-From, V, +To, Out0 <: To] = Summon[From, V, To]{type Out = Out0}
    trait SAM[-From, V, To] extends ExactType.SummonGen[UniqueTag, From, V, To]{type Out = To}
  }
  type Conv[From, To] = ExactType.ConvGen[UniqueTag, From, To]
}

object ExactType {
  @implicitNotFound("Unsupported value type ${V} (supported values are defined by ${From})")
  trait SummonGen[-UniqueTag, -From, V, +To] extends impl.HasOut {
    type Out <: To
    def apply(from : From, value : V) : Out
  }

  trait ConvGen[UniqueTag, From, To] extends impl.HasOut {
    type Out <: To
    def apply(from : From) : Out
  }
  object ConvGen {
    implicit def conv[UniqueTag, From, To, Out <: To](value : Any) : ConvGen[UniqueTag, From, To] =
      macro convMacro[UniqueTag, From, To, Out]
    def convMacro[UniqueTag, From, To, Out <: To](c: whitebox.Context)(value : c.Tree)(
      implicit
      uniqueTagTT : c.WeakTypeTag[UniqueTag],
      fromTT : c.WeakTypeTag[From],
      toTT : c.WeakTypeTag[To]
    ) : c.Tree = {
      import c.universe._
      val uniqueTagTpe = weakTypeOf[UniqueTag]
      val fromTpe = weakTypeOf[From]
      val toTpe = weakTypeOf[To]
      val summonSym = symbolOf[SummonGen[_,_,_,_]]
      val convSym = symbolOf[ConvGen[_,_,_]]
      def exactType(tree : Tree) : c.Type = tree.tpe match {
        case x @ TypeRef(t, sym, _) if sym.fullName.startsWith("scala.Tuple") =>
          val q"$ignore(..$args)" = tree
          internal.typeRef(t, sym, args.map(exactType(_)))
        case x => x
      }
      val valueTpe = exactType(value)
      val fresh = Ident(TermName(c.freshName()))
      val fresh2 = Ident(TermName(c.freshName()))
      val ret = try {
        c.typecheck(q"shapeless.the[$summonSym[$uniqueTagTpe, $fromTpe, $valueTpe, $toTpe]]", silent = false)
      } catch {
        case e : Throwable =>
          val msg = e.getMessage
          val updatedMsg =
            if (msg.startsWith("Unsupported trait")) //workaround leftover message from singleton-ops in some cases
              s"Unsupported value type $valueTpe (supported values are defined by $fromTpe)"
            else msg
          c.abort(c.enclosingPosition, updatedMsg)
      }

      val genTree =
        q"""
        final val $fresh = $ret
        final val $fresh2 = $value.asInstanceOf[$valueTpe]
        new $convSym[$uniqueTagTpe, $fromTpe, $toTpe]{
          type Out = $fresh.Out
          def apply(from : $fromTpe) : Out = $fresh(from, $fresh2)
        }
     """
//          println(genTree)
      genTree
    }
  }

}
