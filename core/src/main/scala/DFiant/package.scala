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

import DFiant.internals._

import scala.language.experimental.macros
import singleton.ops._

package object DFiant extends {
  ////////////////////////////////////////////////////////////////////////////////////
  // A Dataflow Bubble
  ////////////////////////////////////////////////////////////////////////////////////
  sealed trait Bubble
  object Bubble extends Bubble

  type Φ = Bubble
  final val Φ = Bubble
  ////////////////////////////////////////////////////////////////////////////////////

  ////////////////////////////////////////////////////////////////////////////////////
  // Zeros/Ones alternative syntax
  ////////////////////////////////////////////////////////////////////////////////////
  final val b0s = Zeros
  final val b1s = Ones
  ////////////////////////////////////////////////////////////////////////////////////

  ////////////////////////////////////////////////////////////////////////////////////
  // Dataflow Port Annotations
  ////////////////////////////////////////////////////////////////////////////////////
  type <>[DF <: DFAny, Dir <: DFDir] = Dir#Func[DF]
  protected[DFiant] type <~>[DF <: DFAny, Dir <: DFDir] = DFAny.Port[DF, Dir] with DF
  //Direction of a Port
  sealed trait DFDir {
    type Func[DF <: DFAny]
    val isOut : Boolean
    val isIn : Boolean
  }
  implicit object IN extends DFDir {
    type Func[DF <: DFAny] = DF#In
    override def toString: String = "IN"
    final val isOut : Boolean = false
    final val isIn : Boolean = true
  }
  type IN = IN.type
  implicit object OUT extends DFDir {
    type Func[DF <: DFAny] = DF#Out
    override def toString: String = "OUT"
    final val isOut : Boolean = true
    final val isIn : Boolean = false
  }
  type OUT = OUT.type
  ////////////////////////////////////////////////////////////////////////////////////


  ////////////////////////////////////////////////////////////////////////////////////
  // Intervals
  ////////////////////////////////////////////////////////////////////////////////////
  type Interval[T] = continuum.Interval[T]
  final val Interval = continuum.Interval
  type IntervalSet[T] = continuum.IntervalSet[T]
  final val IntervalSet = continuum.IntervalSet
  ////////////////////////////////////////////////////////////////////////////////////


  ////////////////////////////////////////////////////////////////////////////////////
  // List Extender
  ////////////////////////////////////////////////////////////////////////////////////
  trait DFLoopController {
    def run() : Unit
    def stop() : Unit
    def restart() : Unit
  }
  implicit class ListExtender[+T](val list : Iterable[T]) {
//    def foreachdf[W](block : T => Unit)(implicit ctx : DFDesign.Context) : DFLoopController = {
//      import ctx.owner._
//      setFalseNamesInvalidator
//      val lastRun = list.length + 1
//      val sel = DFUInt(BigInt(lastRun).bitsWidth) init 0
//      val runCond = DFBool() init true
//      val matcherFirstCase = matchdf(sel).casedf(0)(block(list.head))
//      val matcherCases = list.drop(1).zipWithIndex.foldLeft(matcherFirstCase)((a, b) => a.casedf(b._2 + 1)(block(b._1)))
//      ifdf(runCond && sel != lastRun) {
//        sel := sel + 1
//      }
//      new DFLoopController {
//        override def run(): Unit = runCond := true
//        override def stop(): Unit = runCond := false
//        override def restart(): Unit = sel := 0
//      }
//    }
    def foreachdf[W](sel : DFUInt[W])(block : PartialFunction[T, Unit])(implicit ctx : ConditionalBlock.Context) : Unit = {
      val blockOwner = ctx.owner
      val blockMatchDF = blockOwner match {
        case o : ConditionalBlock[_,_] => o.owner.asInstanceOf[DFBlock].matchdf
        case _ => blockOwner.matchdf
      }
      val matcherFirstCase = blockMatchDF(sel).casedf(0)(block(list.head))
      val matcherCases = list.drop(1).zipWithIndex.foldLeft(matcherFirstCase)((a, b) => a.casedf(b._2 + 1)(block(b._1)))
    }
    def foreachdf[W](sel : DFBits[W])(block : PartialFunction[T, Unit])(implicit ctx : ConditionalBlock.Context) : Unit = {
      val blockOwner = ctx.owner
      val blockMatchDF = blockOwner match {
        case o : ConditionalBlock[_,_] => o.owner.asInstanceOf[DFBlock].matchdf
        case _ => blockOwner.matchdf
      }
      val matcherFirstCase = blockMatchDF(sel).casedf(BigInt(0).toBitVector(sel.width))(block(list.head))
      val matcherCases = list.drop(1).zipWithIndex.foldLeft(matcherFirstCase)((a, b) => a.casedf(BigInt(b._2 + 1).toBitVector(sel.width))(block(b._1)))
    }
  }

  implicit class MatchList(list : List[(BitVector, BitVector)]) {
    def matchdf[WM, WR](matchValue : DFBits[WM], resultVar : DFBits[WR] with DFAny.Var)(implicit ctx : ConditionalBlock.Context) : Unit = {
      val thisMatchDF = ctx.owner.matchdf
      import ctx.owner._

      if (list.nonEmpty) {
        val matcherFirstCase = thisMatchDF(matchValue).casedf(list.head._1)({resultVar := list.head._2})
        val matcherCases = list.drop(1).foldLeft(matcherFirstCase)((a, b) => a.casedf(b._1)({resultVar := b._2}))
      }
    }
  }

  // Implementation in terms of LabelledGeneric ...
  implicit class MergeSyntax[T](t: T) {
    def mergeWith[U](u: U)(implicit merge: CaseClassMerge[T, U]): T = merge(t, u)
  }
  ////////////////////////////////////////////////////////////////////////////////////

  ////////////////////////////////////////////////////////////////////////////////////
  // BitVector from scodec library https://github.com/scodec/scodec
  // TODO: change after fix for https://github.com/scala/bug/issues/11070
  ////////////////////////////////////////////////////////////////////////////////////
  /*
  Copyright (c) 2013-2014, Michael Pilquist and Paul Chiusano
  All rights reserved.

  Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

  1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
  2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
  3. Neither the name of the scodec team nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
   */
  type XInt = singleton.ops.XInt
  type XBitVector[W] = scodec.bits.BitVector with WidthTag[W]
  type BitVector = scodec.bits.BitVector
  val BitVector = scodec.bits.BitVector

  /**
    * Provides the `b` and `h` string interpolator, which returns `BitVector` instances from binary strings.
    */
  final implicit class BinStringSyntax(val sc: StringContext) {
//    def w[W](args: WidthTag*) : XBitVector[W] = macro Macro.hexStringInterpolator
    /**
      * Converts this binary literal string to a `BitVector`. Whitespace characters are ignored.
      *
      * Named arguments are supported in the same manner as the standard `s` interpolator but they must be
      * of type `BitVector`.
      */
//    def h[W](args: BitVector*) : XBitVector[W] = macro Macro.hexStringInterpolator
    def h[W](args: BitVector*) : BitVector = macro Macro.hexStringInterpolator

    /**
      * Converts this hexadecimal literal string to a `BitVector`. Whitespace characters are ignored.
      *
      * Named arguments are supported in the same manner as the standard `s` interpolator but they must be
      * of type `BitVector`.
      */
    def b[W](args: BitVector*)(implicit interpolator : Interpolator[BitVector]) : interpolator.Out = interpolator()

    def msg(args : Any*)(implicit ctx0 : DFAny.Op.Context) : Message =
      new Message(List(sc.parts,args).flatMap(_.zipWithIndex).sortBy(_._2).map(_._1).filter(p => p match {
        case x: String => x.nonEmpty
        case x => true
      }))
  }

  trait Interpolator[T] {
    type Out <: T
    def apply() : Out
  }

  object Interpolator {
    type Aux[T, Out0 <: T] = Interpolator[T]{type Out = Out0}
    implicit def ev[W] : Interpolator.Aux[BitVector, XBitVector[W]] = macro Macro.binImplStringInterpolator
  }

  protected object Macro {
    object whitebox { type Context = scala.reflect.macros.whitebox.Context }
    def binImplStringInterpolator(c: whitebox.Context) : c.Tree = {
      import c.universe._
      def calcArgsLength(argsTrees : List[Tree]) : Option[Int] = {
        if (argsTrees.isEmpty) Some(0)
        else {
          val tpes = argsTrees.map(e => e.tpe.dealias)
          val lengths : List[Option[Int]] = tpes.collect {
            case RefinedType(parents, scope) => parents.last.typeArgs.head match {
              case ConstantType(Constant(t : Int)) => Some(t)
              case _ => None
            }
            case _ => None
          }
          def sumOption(a : Option[Int], b : Option[Int]) : Option[Int] = (a, b) match {
            case (Some(aa), Some(bb)) => Some(aa + bb)
            case _ => None
          }
          lengths.reduceLeft(sumOption)
        }
      }

      val Apply(TypeApply(Select(properTree,_), _), argsTrees) = c.enclosingImplicits.last.tree
      val args = argsTrees.map(e => c.Expr[BitVector](e))
      val Apply(_, List(Apply(_, parts))) = properTree
      val partLiterals: List[String] = parts map {
        case Literal(Constant(part: String)) =>
          if (BitVector.fromBin(part).isEmpty)
            c.error(c.enclosingPosition, "binary string literal may only contain characters [0, 1]")
          part
      }
      val length = BitVector.fromBin(partLiterals.head).get.length.toInt

      val headPart = c.Expr[String](Literal(Constant(partLiterals.head)))
      val initialStringBuilder = reify { new StringBuilder().append(headPart.splice) }
      val stringBuilder = (args zip partLiterals.tail).foldLeft(initialStringBuilder) {
        case (sb, (arg, part)) =>
          val partExpr = c.Expr[String](Literal(Constant(part)))
          reify { sb.splice.append(arg.splice.toBin).append(partExpr.splice) }
      }
      val buildTree = reify { BitVector.fromValidBin(stringBuilder.splice.toString) }.tree
      val widthTpe : Type = calcArgsLength(argsTrees) match {
        case Some(t) => c.internal.constantType(Constant(length + t))
        case _ => typeOf[Int]
      }
      q"""
         new Interpolator[BitVector] {
           type Out = XBitVector[$widthTpe]
           def apply() : XBitVector[$widthTpe] = $buildTree.asInstanceOf[XBitVector[$widthTpe]]
         }
       """
    }

    def hexStringInterpolator(c: whitebox.Context)(args: c.Expr[BitVector]*): c.Tree = {
      import c.universe._

      val Apply(_, List(Apply(_, parts))) = c.prefix.tree
      val partLiterals: List[String] = parts map {
        case Literal(Constant(part: String)) =>
          if (BitVector.fromHex(part).isEmpty)
            c.error(c.enclosingPosition, "binary string literal may only contain characters [0, 1]")
          part
      }
      val length = BitVector.fromHex(partLiterals.head).get.length.toInt

      val headPart = c.Expr[String](Literal(Constant(partLiterals.head)))
      val initialStringBuilder = reify { new StringBuilder().append(headPart.splice) }
      val stringBuilder = (args zip partLiterals.tail).foldLeft(initialStringBuilder) {
        case (sb, (arg, part)) =>
          val partExpr = c.Expr[String](Literal(Constant(part)))
          reify { sb.splice.append(arg.splice.toBin).append(partExpr.splice) }
      }
      val buildTree = reify { BitVector.fromValidHex(stringBuilder.splice.toString) }.tree
      val widthTpe = c.internal.constantType(Constant(length))
      //      q"$buildTree.asInstanceOf[XBitVector[$widthTpe]]"
      q"$buildTree"
    }
  }

  implicit class XBitVectorExtras[LW](left : XBitVector[LW]) {
    def ##[RW](that : XBitVector[RW])(implicit sum : LW + RW) : XBitVector[sum.OutInt] =
      (left ++ that).asInstanceOf[XBitVector[sum.OutInt]]
  }

  implicit class ProductExtender[P <: Product](p : P) {
    def pipe()(implicit ctx : DFAny.Alias.Context) : P = ???
  }
  ////////////////////////////////////////////////////////////////////////////////////


}
