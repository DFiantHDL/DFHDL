package fpga2019
import DFiant._

trait Fib extends DFDesign {
  val out = DFUInt[64] <> OUT
  val fib = DFUInt[64] init (1, 0)
  fib := fib.prev + fib.prev(2)
  out := fib.prev(2)
}
