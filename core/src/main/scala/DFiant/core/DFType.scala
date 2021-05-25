package DFiant
package core
import compiler.printing.*
import internals.*
import scala.annotation.targetName
import compiletime.*

sealed trait DFType extends NCCode, Product, Serializable:
  protected val width: Int

object DFType:
  trait TC[T]:
    type Type <: DFType
    def apply(t: T): Type

  transparent inline given ofDFType[T <: DFType]: TC[T] =
    new TC[T]:
      type Type = T
      def apply(t: T): Type = t

  extension [T](t: T)(using tc: TC[T])
    def dfType: tc.Type = tc(t)
    def width: Inlined.Int[Width[T]] =
      Inlined.Int.forced[Width[T]](dfType.width)
    def codeString(using Printer): String = dfType.codeString
    def <>(dir: Int): Unit = {}

  import ops.int.+
  protected type Width[T] <: Int = T match
    case EmptyTuple  => 0
    case x *: r      => Width[x] + Width[r]
    case DFBoolOrBit => 1
    case DFBits[w]   => w

  /////////////////////////////////////////////////////////////////////////////
  // DFBool or DFBit
  /////////////////////////////////////////////////////////////////////////////
  sealed trait DFBoolOrBit extends DFType:
    final protected[DFType] val width = 1

  case object DFBool extends DFBoolOrBit:
    def codeString(using Printer): String = "DFBool"
  case object DFBit extends DFBoolOrBit:
    def codeString(using Printer): String = "DFBit"
  /////////////////////////////////////////////////////////////////////////////

  /////////////////////////////////////////////////////////////////////////////
  // DFBits
  /////////////////////////////////////////////////////////////////////////////
  final case class DFBits[W <: Int] private (
      protected[DFType] val width: Int
  ) extends DFType:
    def codeString(using Printer): String = s"DFBits($width)"
  object DFBits:
    def apply[W <: Int](width: Inlined.Int[W]): DFBits[W] = DFBits[W](width)
  /////////////////////////////////////////////////////////////////////////////

  /////////////////////////////////////////////////////////////////////////////
  // DFTuple
  /////////////////////////////////////////////////////////////////////////////
  final case class DFTuple[T <: Tuple] private (dfTypeList: List[DFType])
      extends DFType:
    protected[DFType] val width: Int = dfTypeList.view.map(_.width).sum
    def codeString(using Printer): String =
      dfTypeList.view.map(_.codeString).mkString("(", ", ", ")")

  object DFTuple:
    def apply[T <: Tuple](tuple: T)(using of: Of[T]): DFTuple[T] =
      new DFTuple[T](of(tuple))
    trait Of[T <: Tuple]:
      def apply(t: T): List[DFType]
    object Of:
      import ops.int.+
      import DFType.TC
      // inline given [T <: Tuple]: Of[T] =
      //   new Of[T]:
      //     def apply(t: T): List[DFType] =
      //       t.map[[X] =>> DFType]([T] => (t: T) => summonInline[TC[T]](t))
      //         .toList
      //         .asInstanceOf[List[DFType]]
      inline given [T1, T2](using
          tc1: TC[T1],
          tc2: TC[T2]
      ): Of[(T1, T2)] =
        new Of[(T1, T2)]:
          def apply(t: (T1, T2)): List[DFType] = List(tc1(t._1), tc2(t._2))

  transparent inline given ofTuple[T <: Tuple](using of: DFTuple.Of[T]): TC[T] =
    new TC[T]:
      type Type = DFTuple[T]
      def apply(t: T): Type = DFTuple(t)

/////////////////////////////////////////////////////////////////////////////

val w: Inlined.Int[8] = DFBits(8).width
val w2: Inlined.Int[16] = (DFBits(8), DFBits(8)).width
val w3: Inlined.Int[25] = ((DFBits(8), DFBit), (DFBits(8), DFBits(8))).width
val x = (DFBits(8), DFBits(8))

import DFType.TC

val xx = summonInline[TC[DFBits[8]]]
