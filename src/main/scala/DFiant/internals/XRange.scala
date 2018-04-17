package DFiant.internals

import singleton.ops._

trait XRangeTag[Start, End]
//case class XRange[Start, End](start : Start, end : End)
//object XRange {
//  implicit def toRange[Start <: Int with Singleton, End <: Int with Singleton](xrange : XRange[Start, End])(
//    implicit start: ValueOf[Start], end: ValueOf[End]
//  ) : Range = Range(valueOf[Start], valueOf[End])
//}
