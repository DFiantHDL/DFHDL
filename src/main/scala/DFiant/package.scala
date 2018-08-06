import scala.language.experimental.macros


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
  // Dataflow Port Annotations
  ////////////////////////////////////////////////////////////////////////////////////
  type <>[DF <: DFAny, Dir <: DFDir] = DFAny.Port[DF, Dir] with DF
  //Direction of a Port
  sealed trait DFDir {
    val isOut : Boolean
    val isIn : Boolean
  }
  sealed trait IN extends DFDir {
    override def toString: String = "IN"
    final val isOut : Boolean = false
    final val isIn : Boolean = true
  }
  implicit object IN extends IN
  sealed trait OUT extends DFDir {
    override def toString: String = "OUT"
    final val isOut : Boolean = true
    final val isIn : Boolean = false
  }
  implicit object OUT extends OUT
  ////////////////////////////////////////////////////////////////////////////////////


  ////////////////////////////////////////////////////////////////////////////////////
  // BitVector from scodec library https://github.com/scodec/scodec
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
  type BitVector = scodec.bits.BitVector
  val BitVector = scodec.bits.BitVector
  type ByteVector = scodec.bits.ByteVector
  val ByteVector = scodec.bits.ByteVector

  /**
    * Provides the `bin` string interpolator, which returns `BitVector` instances from binary strings.
    */
  final implicit class BinStringSyntax(val sc: StringContext) extends AnyVal {

    /**
      * Converts this binary literal string to a `BitVector`. Whitespace characters are ignored.
      *
      * Named arguments are supported in the same manner as the standard `s` interpolator but they must be
      * of type `BitVector`.
      */
    def bin(args: BitVector*): BitVector = macro scodec.bits.LiteralSyntaxMacros.binStringInterpolator
  }

  /**
    * Provides the `hex` string interpolator, which returns `ByteVector` instances from hexadecimal strings.
    */
  final implicit class HexStringSyntax(val sc: StringContext) extends AnyVal {

    /**
      * Converts this hexadecimal literal string to a `ByteVector`. Whitespace characters are ignored.
      *
      * Named arguments are supported in the same manner as the standard `s` interpolator but they must be
      * of type `ByteVector`.
      */
    def hex(args: ByteVector*): ByteVector = macro scodec.bits.LiteralSyntaxMacros.hexStringInterpolator
  }
  ////////////////////////////////////////////////////////////////////////////////////


}
