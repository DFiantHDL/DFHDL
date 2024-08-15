package dfhdl.lib.arith

import dfhdl.*
import internals.{IntInfo, -}

@hw.pure
def prioEncRecur(value: Bits[Int] <> VAL): (Bit, Bits[Int]) <> DFRET =
  val width = value.widthInt
  if (width == 2) (value(1) || value(0), value(1, 1))
  else
    val lsHalf = width / 2
    val msHalf = width - lsHalf
    val half = lsHalf max msHalf
    val lsPrio = prioEncRecur(value.lsbits(lsHalf).resize(half))
    val msPrio = prioEncRecur(value.msbits(msHalf).resize(half))
    val selPrio = if (msPrio._1) msPrio._2 else lsPrio._2
    (msPrio._1 || lsPrio._1, (msPrio._1, selPrio))

@inline def prioEnc[W <: Int](value: Bits[W] <> VAL)(using
    info: IntInfo[W - 1]
): (Bit, Bits[info.OutW]) <> DFRET =
  require(
    value.widthInt > 1,
    s"Priority encoded value width must be larger than 1. Found: ${value.widthInt}"
  )
  prioEncRecur(value).asValOf[(Bit, Bits[info.OutW])]
