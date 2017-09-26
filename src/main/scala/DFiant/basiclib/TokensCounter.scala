package DFiant.basiclib

import DFiant.core._

//object TokensCounter {
//  def apply(in : DFAny, supremLimit : Int) : DFUInt = {
//    val cnt = DFUInt.rangeUntil(supremLimit-1).initPrev(0)
//
//    ifdf (in.isNotEmpty) {
//      ifdf (cnt == supremLimit-1) {
//        cnt := 0
//      } elsedf {
//        cnt := cnt + 1
//      }
//    } elsedf {
//      in.dontConsume()
//      cnt.dontProduce() //cnt.prev preserves latest valid value
//    }
//    cnt
//  }
//
//}
