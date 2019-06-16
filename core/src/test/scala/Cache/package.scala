/*
 *     This file is part of DFiant.
 *
 *     DFiant is free software: you can redistribute it and/or modify
 *     it under the terms of the Lesser GNU General Public License as published by
 *     the Free Software Foundation, either version 3 of the License, or
 *     any later version.
 *
 *     DFiant is distributed in the hope that it will be useful,
 *     but WITHOUT ANY WARRANTY; without even the implied warranty of
 *     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *     Lesser GNU General Public License for more details.
 *
 *     You should have received a copy of the Lesser GNU General Public License
 *     along with DFiant.  If not, see <https://www.gnu.org/licenses/>.
 */

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
