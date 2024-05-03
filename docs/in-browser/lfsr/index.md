---
hide:
  - toc
---

# LFSR

```scastie 
import dfhdl.*

enum Ctrl extends Encode:
  case Idle, Seed, Run

/** Galois Linear-Feedback Shift Register
  */
class LFSR(val taps: Bits[Int] <> CONST) extends RTDesign:
  val ctrl = Ctrl <> IN
  val seed = Bits(taps.width) <> IN
  val calc = Bits(taps.width) <> OUT init all(1)

  import Ctrl.*
  ctrl match
    case Idle => // do nothing
    case Seed =>
      if (seed == all(0)) calc := all(1)
      else calc := seed
    case Run =>
      val tap = if (calc.reg(1)(0)) taps else b"${taps.width}'0"
      calc := (calc.reg >> 1) ^ tap
end LFSR
```

