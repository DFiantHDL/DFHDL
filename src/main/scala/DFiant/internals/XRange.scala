package DFiant.internals

import singleton.ops._
import singleton.ops.impl.std
import singleton.twoface._

object XRange {
  protected trait IntTag[Start, End]
  type Int[Start <: std.Int, End <: std.Int] = Range with IntTag[Start, End]
  protected trait LongTag[Start, End]
  type Long[Start <: std.Long, End <: std.Long] = Range with LongTag[Start, End]

  protected object Int {
    def apply[Start <: std.Int, End <: std.Int](start : Start, end : End)
    : Int[Start, End] = Range(start, end).asInstanceOf[Int[Start, End]]
  }

  protected object Long {
    def apply[Start <: std.Long, End <: std.Long](start : Start, end : End)
    : Long[Start, End] = Range.Long(start, end, 1L).asInstanceOf[Long[Start, End]]
  }

  object Check extends Checked1Param.Int {
    type Cond[S, E] = S <= E
    type Msg[S, E] = "Empty Range"
    type ParamFace = std.Int
  }
  trait Implicits {
    implicit class fromInt[Start <: std.Int](start : Start) {
      def TO[End <: std.Int, S <: std.Int, E <: std.Int](end : End)(
        implicit s : OpAuxGen[AcceptNonLiteral[GetLHSArg0], S],
        e : OpAuxGen[AcceptNonLiteral[GetArg0], E],
        check : Check.CheckedShell[S, E]
      ) : Int[S, E] = {
        check.unsafeCheck(start, end)
        XRange.Int[S, E](s.value, e.value)
      }
//      def UNTIL[End <: std.Int with Singleton](end : End)(
//        implicit check : RequireMsg[Start < End, "Empty Range"], e : SafeInt[End - 1]
//      ) : Int[Start, e.Out] = XRange.Int[Start, e.Out](start, e.value)
//      def DOWNTO[End <: std.Int with Singleton](end : End)(
//        implicit check : RequireMsg[End <= Start, "Empty Range"]
//      ) : Int[End, Start] = XRange.Int[End, Start](end, start)
//      def DOWNTIL[End <: std.Int with Singleton](end : End)(
//        implicit check : RequireMsg[End < Start, "Empty Range"], e : SafeInt[End + 1]
//      ) : Int[e.Out, Start] = XRange.Int[e.Out, Start](e.value, start)
    }

  }

}


//case class XRange[Start, End](start : Start, end : End)
//object XRange {
//  implicit def toRange[Start <: Int with Singleton, End <: Int with Singleton](xrange : XRange[Start, End])(
//    implicit start: ValueOf[Start], end: ValueOf[End]
//  ) : Range = Range(valueOf[Start], valueOf[End])
//}
