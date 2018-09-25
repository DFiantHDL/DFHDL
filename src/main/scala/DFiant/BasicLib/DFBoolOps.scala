package DFiant.BasicLib
import DFiant._
import internals._

object DFBoolOps {
  class BoolopBool[Kind <: DiSoOp.Kind](
    implicit ctx : DFComponent.Context[BoolopBool[Kind]], kind : Kind
  ) extends DFComponent[BoolopBool[Kind]] {
    final val inLeft = DFBool() <> IN
    final val inRight = DFBool() <> IN
    final val outResult = DFBool() <> OUT
    override protected def foldedRun: Unit = {
      val func = kind match {
        case _: DiSoOp.Kind.|| => DFBool.Token.||
        case _: DiSoOp.Kind.&& => DFBool.Token.&&
        case _: DiSoOp.Kind.== => DFBool.Token.==
        case _: DiSoOp.Kind.!= => DFBool.Token.!=
        case _ => throw new IllegalArgumentException("Unexptected operation")
      }
      setInitFunc(outResult)(LazyBox.Args2(fullName)(func, getInit(inLeft), getInit(inRight)))
    }
    final protected val foldedDiscoveryDependencyList = (outResult -> (inLeft :: inRight :: Nil)) :: Nil
  }

  type `Comp||` = BoolopBool[DiSoOp.Kind.||]
  type `Comp&&` = BoolopBool[DiSoOp.Kind.&&]
  type `Comp==` = BoolopBool[DiSoOp.Kind.==]
  type `Comp!=` = BoolopBool[DiSoOp.Kind.!=]
}
