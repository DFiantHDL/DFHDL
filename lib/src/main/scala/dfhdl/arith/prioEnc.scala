package dfhdl.lib.arith

import dfhdl.*
import core.asValOf
import internals.IntInfo

def prioEncRecur(value: Bits[Int] <> VAL): Bits[Int] <> DFRET =
  val width = value.width
  if (width == 1) value
  else
    val selPrio =
      if (value.msbit) prioEncRecur(value.msbits(width / 2))
      else prioEncRecur(value.lsbits(width - width / 2))
    (value.msbit, selPrio)

@inline def prioEnc[W <: Int](value: Bits[W] <> VAL)(using
    info: IntInfo[W]
): Bits[info.OutW] <> DFRET =
  prioEncRecur(value).asValOf[Bits[info.OutW]]
