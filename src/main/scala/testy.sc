import DFiant._
import singleton.ops._
type DFDouble = DFUInt[W.`64`.T]


val windowLength = 1024
//def inverse(data : DFDouble) : DFDouble = {
//  val cnt = DFUInt(10)
//  cnt := cnt + 1
//  val norm = (1.0/(windowLength.toDouble/2))
//  if (cnt < windowLength) {
//    data * norm
//  } else {
//    data
//  }
//}


def inverse2(data : DFDouble) : DFDouble = {
  val norm = (1.0/(windowLength.toDouble/2))
  val ds = data.split(windowLength)(2)
  val (f, l) = (ds.head, ds.last)
  f
}




