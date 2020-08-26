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

import DFiant.internals._
import DFiant.compiler._
import backend.BackendStage
import csprinter.{CSPrinter, CompactCodeString, PrinterOps}

import scala.language.experimental.macros
import singleton.ops._
import singleton.ops.impl.HasOut
import DFiant.sim._

package object DFiant {
  type DFBits[W] = DFAny.Of[DFBits.Type[W]]
  type DFBool = DFAny.Of[DFBool.Type]
  type DFBit = DFAny.Of[DFBool.Type]
  type DFUInt[W] = DFAny.Of[DFUInt.Type[W]]
  type DFSInt[W] = DFAny.Of[DFSInt.Type[W]]
  type DFEnum[E <: EnumType] = DFAny.Of[DFEnum.Type[E]]
//  type DFString[L] = DFAny.Of[DFString.Type[L]]

  implicit def evPrinterOps[D <: DFDesign, C](c : C)(implicit conv : C => Compilation[D])
  : PrinterOps[D, C] = new PrinterOps[D, C](c)
  implicit class evAddTagOps[D <: DFDesign, C](c : C)(
    implicit conv : C => IRCompilation[D], externalExtension: ExternalExtension
  ) {
    def !!(tags : TagsOf[D]) = new AddTags[D](conv(c)).addTags(tags)
  }

  implicit class BackendExt[D <: DFDesign, T](t : T)(
    implicit conv : T => IRCompilation[D]
  ) {
    def compile[B <: BackendStage](
      implicit preCompiler : PreCompiler[D], compiler : BackendStage.Compiler[B], postCompiler : PostCompiler[D, B]
    ) : BackendStage.Compilation[D, B] = postCompiler(compiler(preCompiler(t)))
  }
  implicit class SimulatorExt[D <: DFSimDesign, B <: BackendStage](c : BackendStage.CommittedCompilation[D, B]) {
    def simulation[S <: Simulation[D, B]](implicit simulator : Simulator[D, B, S]) : S = simulator(c)
  }

  ////////////////////////////////////////////////////////////////////////////////////
  // A Dataflow Bubble
  ////////////////////////////////////////////////////////////////////////////////////
  sealed trait Bubble
  object Bubble extends Bubble {
    sealed trait Behaviour extends Product with Serializable
    implicit case object Stall extends Behaviour
    case object DontCare extends Behaviour
  }

  type ? = Bubble
  final val ? = Bubble
  ////////////////////////////////////////////////////////////////////////////////////

  ////////////////////////////////////////////////////////////////////////////////////
  // Dataflow Port Annotations
  ////////////////////////////////////////////////////////////////////////////////////
  sealed trait DFDir extends Product with Serializable {
    type Func[DF <: DFAny]
  }
  type <>[DF <: DFAny, Dir <: DFDir] = Dir#Func[DF]
//  protected[DFiant] type <~>[DF <: DFAny, Dir <: DFDir] = DFAny.Port[DF#TType, Dir]

  //Declaration directionality (Var/PortDir)
  sealed trait DclDir extends DFDir
  //Direction of a Port
  sealed trait PortDir extends DclDir
  case object IN extends PortDir {
    type Func[DF <: DFAny] = DFAny.DefaultRet[DF#TType]
    override def toString: String = "IN "
  }
  type IN = IN.type
  case object OUT extends PortDir {
    type Func[DF <: DFAny] = DFAny.VarOf[DF#TType]
    override def toString: String = "OUT"
  }
  type OUT = OUT.type
  case object VAR extends DclDir {
    type Func[DF <: DFAny] = DFAny.VarOf[DF#TType]
  }
  type VAR = VAR.type
  case object FLIP extends DFDir
  case object ASIS extends DFDir
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
  /**
    * Provides the `b` and `h` string interpolator, which returns `BitVector` instances from binary strings.
    */
  final implicit class BinStringSyntax(val sc: StringContext) {
    def b[W](args: Any*)(implicit interpolator : Interpolator[DFBits.Token, "b"]) : interpolator.Out = interpolator.value
    def h[W](args: Any*)(implicit interpolator : Interpolator[DFBits.Token, "h"]) : interpolator.Out = interpolator.value

    private def commonInterpolation(args : Seq[Any]) : Seq[Either[DFAny, String]] =
      Seq(sc.parts,args).flatMap(_.zipWithIndex).sortBy(_._2).map(_._1).filter(p => p match {
        case x: String => x.nonEmpty
        case _ => true
      }).map {
        case x : DFAny => Left(x)
        case x => Right(x.toString)
      }
    def msg(args : Any*) : DFSimMember.Assert.Message = DFSimMember.Assert.Message(commonInterpolation(args))
    def cs(args : Any*)(implicit ctx : DFAny.Context) : CompactCodeString = {
      val seq = Seq(sc.parts,args).flatMap(_.zipWithIndex).sortBy(_._2).map(_._1).filter(p => p match {
        case x: String => x.nonEmpty
        case _ => true
      }).map {
        case x : DFAny => CompactCodeString.MemberPart(x)
        case CSFunc(func) => CompactCodeString.CSPrintPart(func)
        case x => CompactCodeString.StringPart(x.toString)
      }
      CompactCodeString(seq)
    }
    def vhdl(args : Any*)(implicit ctx : DFAny.Context) : BackendEmitter = BackendEmitter(commonInterpolation(args), compiler.backend.vhdl.Backend)
    def verilog(args : Any*)(implicit ctx : DFAny.Context) : BackendEmitter = BackendEmitter(commonInterpolation(args), compiler.backend.verilog.Backend)
  }
  trait Interpolator[T, K] extends HasOut {
    type Out <: T
    val value : Out
  }

  object Interpolator {
    type Aux[T, K, Out0 <: T] = Interpolator[T, K]{type Out = Out0}
    implicit def evb[W] : Interpolator.Aux[DFBits.Token, "b", DFBits.TokenW[W]] = macro DFBits.Token.binImplStringInterpolator
    implicit def evh[W] : Interpolator.Aux[DFBits.Token, "h", DFBits.TokenW[W]] = macro DFBits.Token.hexImplStringInterpolator
  }
  final case class CSFunc(func : CSPrinter.Config => String)
  ////////////////////////////////////////////////////////////////////////////////////


  ////////////////////////////////////////////////////////////////////////////////////
  // Conditional Constructs
  ////////////////////////////////////////////////////////////////////////////////////
  def ifdf[C](cond : C)(block : => Unit)(
    implicit ctx : DFBlock.Context, condArg : DFBool.Arg[0]
  ) : ConditionalBlock.NoRetVal.IfElseBlock[true] =
    new ConditionalBlock.NoRetVal.IfElseBlock[true](Some(condArg()), None)(block)
  def matchdf[MVType <: DFAny.Type](matchValue : DFAny.Of[MVType], matchConfig : MatchConfig = MatchConfig.NoOverlappingCases)(
    implicit ctx : DFBlock.Context
  ): ConditionalBlock.NoRetVal.MatchHeader[MVType] =
    new ConditionalBlock.NoRetVal.MatchHeader[MVType](matchValue, matchConfig)

  implicit class ListExtender[+T](val list : Iterable[T]) {
    def foreachdf[W](sel : DFUInt[W])(block : PartialFunction[T, Unit])(implicit ctx : DFBlock.Context) : Unit = {
      val blockMatchDF = new ConditionalBlock.NoRetVal.MatchHeader[DFUInt.Type[W]](sel, MatchConfig.NoOverlappingCases)
      val matcherFirstCase = blockMatchDF.casedf(0)(block(list.head))
      val cases = list.drop(1).zipWithIndex.foldLeft(matcherFirstCase)((a, b) => a.casedf(b._2 + 1)(block(b._1)))
      if (sel.width.getValue != (list.size-1).bitsWidth) cases.casedf_{}
    }
    def foreachdf[W](sel : DFBits[W])(block : PartialFunction[T, Unit])(implicit ctx : DFBlock.Context, di : DummyImplicit) : Unit = {
      val blockMatchDF = new ConditionalBlock.NoRetVal.MatchHeader[DFBits.Type[W]](sel, MatchConfig.NoOverlappingCases)
      val matcherFirstCase = blockMatchDF.casedf(DFBits.Token(sel.width.getValue, BigInt(0)))(block(list.head))
      val cases = list.drop(1).zipWithIndex.foldLeft(matcherFirstCase)((a, b) => a.casedf(DFBits.Token(sel.width.getValue, BigInt(b._2 + 1)))(block(b._1)))
      if (sel.width.getValue != (list.size-1).bitsWidth) cases.casedf_{}
    }
  }

  implicit class MatchList(list : List[(DFBits.Token, DFBits.Token)]) {
    import DFDesign.Implicits._
    def matchdf[MW, RW](matchValue : DFBits[MW], resultVar : DFAny.VarOf[DFBits.Type[RW]])(implicit ctx : DFBlock.Context) : Unit = {
      val blockMatchDF = new ConditionalBlock.NoRetVal.MatchHeader[DFBits.Type[MW]](matchValue, MatchConfig.NoOverlappingCases)
      if (list.nonEmpty) {
        val matcherFirstCase = blockMatchDF.casedf(list.head._1){resultVar := list.head._2}
        val cases = list.drop(1).foldLeft(matcherFirstCase)((a, b) => a.casedf(b._1){resultVar := b._2})
        if (matchValue.width.getValue != (list.size-1).bitsWidth) cases.casedf_{}
      }
    }
  }
  ////////////////////////////////////////////////////////////////////////////////////

}
