package DFiant.core

import singleton.ops._

//@scala.annotation.implicitNotFound("Low bit index ${L} is bigger than High bit index ${H}")
//trait ChkBitsRange[H, L] {
//  type Width
//}
//object ChkBitsRange {
//  implicit def ev[H, L](
//    implicit
//    check : CompileTime[L <= H],
//    w : FallBack[Int, Int, H-L+1]
//  ) : ChkBitsRange[H, L] {type Width = w.Out} = new ChkBitsRange[H, L] {type Width = w.Out}
//}
