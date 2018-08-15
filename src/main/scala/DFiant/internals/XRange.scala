package DFiant.internals

import singleton.ops._
import singleton.ops.impl.std
import singleton.twoface._

object XRange {
  protected trait IntTag[Start, End]
  type Int[Start <: std.Int, End <: std.Int] = Range with IntTag[Start, End]
  protected trait LongTag[Start, End]
  type Long[Start <: std.Long, End <: std.Long] = Range with LongTag[Start, End]
  trait TO
  trait DOWNTO

  protected object Int {
    type TO[Start <: std.Int, End <: std.Int] = Int[Start, End] with XRange.TO
    def TO[Start <: std.Int, End <: std.Int](start : Start, end : End)
    : TO[Start, End] = Range(start, end).asInstanceOf[TO[Start, End]]
    type DOWNTO[Start <: std.Int, End <: std.Int] = Int[Start, End] with XRange.DOWNTO
    def DOWNTO[Start <: std.Int, End <: std.Int](start : Start, end : End)
    : DOWNTO[Start, End] = Range(start, end, -1).asInstanceOf[DOWNTO[Start, End]]
    object Check extends Checked1Param.Int {
      type Cond[S, E] = S <= E
      type Msg[S, E] = "Empty Range"
      type ParamFace = std.Int
    }
  }

  protected object Long {
    type TO[Start <: std.Long, End <: std.Long] = Long[Start, End] with XRange.TO
    def TO[Start <: std.Long, End <: std.Long](start : Start, end : End)
    : TO[Start, End] = Range.Long(start, end, 1L).asInstanceOf[TO[Start, End]]
    type DOWNTO[Start <: std.Long, End <: std.Long] = Long[Start, End] with XRange.DOWNTO
    def DOWNTO[Start <: std.Long, End <: std.Long](start : Start, end : End)
    : DOWNTO[Start, End] = Range.Long(start, end, -1L).asInstanceOf[DOWNTO[Start, End]]
    object Check extends Checked1Param.Long {
      type Cond[S, E] = S <= E
      type Msg[S, E] = "Empty Range"
      type ParamFace = std.Long
    }
  }

  trait Implicits {
    final class XRangefromInt[Start <: std.Int](start : Start) {
      def TO[End <: std.Int, S <: std.Int, E <: std.Int](end : End)(
        implicit s : OpAuxGen[AcceptNonLiteral[GetLHSArg0], S],
        e : OpAuxGen[AcceptNonLiteral[GetArg0], E],
        check : Int.Check.CheckedShell[S, E]
      ) : Int.TO[S, E] = {
        check.unsafeCheck(start, end)
        Int.TO[S, E](s.value, e.value)
      }
      def UNTIL[End <: std.Int, S <: std.Int, E <: std.Int](end : End)(
        implicit s : OpAuxGen[AcceptNonLiteral[GetLHSArg0], S],
        e : OpAuxGen[AcceptNonLiteral[GetArg0-1], E],
        check : Int.Check.CheckedShell[S, E]
      ) : Int.TO[S, E] = {
        check.unsafeCheck(start, end)
        Int.TO[S, E](s.value, e.value)
      }
      def DOWNTO[End <: std.Int, S <: std.Int, E <: std.Int](end : End)(
        implicit s : OpAuxGen[AcceptNonLiteral[GetLHSArg0], S],
        e : OpAuxGen[AcceptNonLiteral[GetArg0], E],
        check : Int.Check.CheckedShell[E, S]
      ) : Int.DOWNTO[S, E] = {
        check.unsafeCheck(start, end)
        Int.DOWNTO[S, E](s.value, e.value)
      }
      def DOWNTIL[End <: std.Int, S <: std.Int, E <: std.Int](end : End)(
        implicit s : OpAuxGen[AcceptNonLiteral[GetLHSArg0], S],
        e : OpAuxGen[AcceptNonLiteral[GetArg0+1], E],
        check : Int.Check.CheckedShell[E, S]
      ) : Int.DOWNTO[S, E] = {
        check.unsafeCheck(start, end)
        Int.DOWNTO[S, E](s.value, e.value)
      }
    }
    final implicit def XRangefromInt[Start <: std.Int](start: Start): XRangefromInt[Start] = XRangefromInt(start)

    final class XRangefromLong[Start <: std.Long](start : Start) {
      def TO[End <: std.Long, S <: std.Long, E <: std.Long](end : End)(
        implicit s : OpAuxGen[AcceptNonLiteral[GetLHSArg0], S],
        e : OpAuxGen[AcceptNonLiteral[GetArg0], E],
        check : Long.Check.CheckedShell[S, E]
      ) : Long.TO[S, E] = {
        check.unsafeCheck(start, end)
        Long.TO[S, E](s.value, e.value)
      }
      def UNTIL[End <: std.Long, S <: std.Long, E <: std.Long](end : End)(
        implicit s : OpAuxGen[AcceptNonLiteral[GetLHSArg0], S],
        e : OpAuxGen[AcceptNonLiteral[GetArg0-1], E],
        check : Long.Check.CheckedShell[S, E]
      ) : Long.TO[S, E] = {
        check.unsafeCheck(start, end)
        Long.TO[S, E](s.value, e.value)
      }
      def DOWNTO[End <: std.Long, S <: std.Long, E <: std.Long](end : End)(
        implicit s : OpAuxGen[AcceptNonLiteral[GetLHSArg0], S],
        e : OpAuxGen[AcceptNonLiteral[GetArg0], E],
        check : Long.Check.CheckedShell[E, S]
      ) : Long.DOWNTO[S, E] = {
        check.unsafeCheck(start, end)
        Long.DOWNTO[S, E](s.value, e.value)
      }
      def DOWNTIL[End <: std.Long, S <: std.Long, E <: std.Long](end : End)(
        implicit s : OpAuxGen[AcceptNonLiteral[GetLHSArg0], S],
        e : OpAuxGen[AcceptNonLiteral[GetArg0+1], E],
        check : Long.Check.CheckedShell[E, S]
      ) : Long.DOWNTO[S, E] = {
        check.unsafeCheck(start, end)
        Long.DOWNTO[S, E](s.value, e.value)
      }
    }
    final implicit def XRangefromLong[Start <: std.Long](start: Start): XRangefromLong[Start] = XRangefromLong(start)
  }
}

