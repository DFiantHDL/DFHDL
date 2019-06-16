/*
 *     This file is part of DFiant.
 *
 *     DFiant is free software: you can redistribute it and/or modify
 *     it under the terms of the Lesser GNU General Public License as published by
 *     the Free Software Foundation, either version 3 of the License, or
 *     any later version.
 *
 *     DFiant is distributed in the hope that it will be useful,
 *     but WITHOUT ANY WARRANTY; without even the implied warranty of
 *     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *     Lesser GNU General Public License for more details.
 *
 *     You should have received a copy of the Lesser GNU General Public License
 *     along with DFiant.  If not, see <https://www.gnu.org/licenses/>.
 */

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
        case _: DiSoOp.Kind.^  => DFBool.Token.^
        case _: DiSoOp.Kind.== => DFBool.Token.==
        case _: DiSoOp.Kind.!= => DFBool.Token.!=
        case _ => throw new IllegalArgumentException("Unexptected operation")
      }
      setInitFunc(outResult)(LazyBox.Args2(this)(func, getInit(inLeft), getInit(inRight)))
    }
    final protected val foldedDiscoveryDependencyList = (outResult -> (inLeft :: inRight :: Nil)) :: Nil
  }

  type `Comp||` = BoolopBool[DiSoOp.Kind.||]
  type `Comp&&` = BoolopBool[DiSoOp.Kind.&&]
  type `Comp^`  = BoolopBool[DiSoOp.Kind.^]
  type `Comp==` = BoolopBool[DiSoOp.Kind.==]
  type `Comp!=` = BoolopBool[DiSoOp.Kind.!=]
}
