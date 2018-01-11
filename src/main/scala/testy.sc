import DFiant._
import singleton.ops._
type DFDouble = DFUInt[W.`64`.T]


val windowLength = 1024
def inverse(data : DFDouble) : DFDouble = {
  val norm = (1.0/(windowLength.toDouble/2))
  data * norm
}



