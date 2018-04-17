import DFiant.internals._
import singleton.ops._
package object DFiant extends {
  sealed trait Bubble
  object Bubble extends Bubble

  type Φ = Bubble
  final val Φ = Bubble

  type XRange[Start, End] = Range with XRangeTag[Start, End]
  implicit class XIntExtras[Start <: Int with Singleton](start : Start) {
    def TO[End <: Int with Singleton](end : End)(
      implicit check : RequireMsg[Start <= End, "Empty Range"]
    ) : XRange[Start, End] = Range(start, end).asInstanceOf[XRange[Start, End]]
//    def Until[End <: Int with Singleton](end : End)(
//      implicit check : RequireMsg[Start < End, "Empty Range"], e : SafeInt[End - 1]
//    ) : XRange[Start, e.Out] = XRange[Start, e.Out](start, e.value)
//    def Downto[End <: Int with Singleton](end : End)(
//      implicit check : RequireMsg[End <= Start, "Empty Range"]
//    ) : XRange[End, Start] = XRange[End, Start](end, start)
//    def Downtil[End <: Int with Singleton](end : End)(
//      implicit check : RequireMsg[End < Start, "Empty Range"], e : SafeInt[End + 1]
//    ) : XRange[e.Out, Start] = XRange[e.Out, Start](e.value, start)
  }

}
