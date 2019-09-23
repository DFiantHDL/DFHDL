/*
 *     This file is part of DFiant.
 *
 *     DFiant is free software: you can redistribute it and/or modify
 *     it under the terms of the GNU Lesser General Public License as published by
 *     the Free Software Foundation, either version 3 of the License, or
 *     any later version.
 *
 *     DFiant is distributed in the hope that it will be useful,
 *     but WITHOUT ANY WARRANTY; without even the implied warranty of
 *     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *     GNU Lesser General Public License for more details.
 *
 *     You should have received a copy of the GNU Lesser General Public License
 *     along with DFiant.  If not, see <https://www.gnu.org/licenses/>.
 */

package DFiant.targetlib
import DFiant._
import internals._

object DFBitsOps {
  class Bitwise[Kind <: DiSoOp.Kind](
    val leftWidth : Int, val rightWidth : Int, val resultWidth : Int)(
    implicit ctx : DFComponent.Context[Bitwise[Kind]], kind : Kind
  ) extends DFComponent[Bitwise[Kind]] {
    final val inLeft = DFBits(leftWidth) <> IN
    final val inRight = DFBits(rightWidth) <> IN
    final val outResult = DFBits(resultWidth) <> OUT
    final override protected val blackBoxFunctions = Map(outResult -> BlackBoxFunction(outResult)(inLeft, inRight){
      (l, r) => kind match {
        case _: DiSoOp.Kind.| => l | r
        case _: DiSoOp.Kind.& => l & r
        case _: DiSoOp.Kind.^ => l ^ r
        case _ => throw new IllegalArgumentException("Unexptected operation")
      }
    })
  }

  type `Comp|` = Bitwise[DiSoOp.Kind.|]
  type `Comp&` = Bitwise[DiSoOp.Kind.&]
  type `Comp^` = Bitwise[DiSoOp.Kind.^]

  class Relational[Kind <: DiSoOp.Kind](
    val leftWidth : Int, val rightWidth : Int)(
    implicit ctx : DFComponent.Context[Relational[Kind]], kind : Kind
  ) extends DFComponent[Relational[Kind]] {
    final val inLeft = DFBits(leftWidth) <> IN
    final val inRight = DFBits(rightWidth) <> IN
    final val outResult = DFBool() <> OUT
    final override protected val blackBoxFunctions = Map(outResult -> BlackBoxFunction(outResult)(inLeft, inRight){
      (l, r) => kind match {
        case _: DiSoOp.Kind.== => l == r
        case _: DiSoOp.Kind.!= => l != r
        case _ => throw new IllegalArgumentException("Unexptected operation")
      }
    })
  }

  type `Comp==` = Relational[DiSoOp.Kind.==]
  type `Comp!=` = Relational[DiSoOp.Kind.!=]

  class BitShift[Kind <: DiSoOp.Kind](
    val leftWidth : Int, val rightWidth : Int)(
    implicit ctx : DFComponent.Context[BitShift[Kind]], kind : Kind
  ) extends DFComponent[BitShift[Kind]] {
    final val inLeft = DFBits(leftWidth) <> IN
    final val inRight = DFUInt(rightWidth) <> IN
    final val outResult = DFBits(leftWidth) <> OUT
    final override protected val blackBoxFunctions = Map(outResult -> BlackBoxFunction(outResult)(inLeft, inRight){
      (l, r) => kind match {
        case _: DiSoOp.Kind.<< => l << r
        case _: DiSoOp.Kind.>> => l >> r
        case _ => throw new IllegalArgumentException("Unexptected operation")
      }
    })
  }

  type `Comp<<` = BitShift[DiSoOp.Kind.<<]
  type `Comp>>` = BitShift[DiSoOp.Kind.>>]
}
