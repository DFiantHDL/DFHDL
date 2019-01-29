import DFiant._
import internals._
package object Cache {
  final val AddrSize = 32
  final val Rows = 256
  final val DataSize = 32

  final def Addr()(implicit ctx : DFAny.NewVar.Context) = DFBits(AddrSize)
  type Addr = DFBits[AddrSize.type]
  final def Index()(implicit ctx : DFAny.NewVar.Context) = DFBits(Rows.bitsWidth)
  final def Tag()(implicit ctx : DFAny.NewVar.Context) = DFBits(AddrSize-Rows.bitsWidth-2)
  final def Data()(implicit ctx : DFAny.NewVar.Context) = DFBits(DataSize)

  implicit object CacheStatus extends Enum.Auto {
    val Rdy, WrBack, Fill = Entry()
  }
  type CacheStatus = CacheStatus.type

  implicit object MemOp extends Enum.Auto {
    val Ld, St = Entry()
  }
  type MemOp = MemOp.type

//  final val DFArray()
}
