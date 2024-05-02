---
hide:
  - toc
---

# N-Bit Full Adder

```scastie 
import dfhdl.*

class FullAdder1 extends EDDesign:
  val a, b, c_in = Bit <> IN
  val sum, c_out = Bit <> OUT

  sum <> (a ^ b ^ c_in)
  c_out <> (a && b || b && c_in || c_in && a)

class FullAdderN(n: Int) extends EDDesign:
  val a, b  = Bits(n) <> IN
  val c_in  = Bit     <> IN
  val sum   = Bits(n) <> OUT
  val c_out = Bit     <> OUT

  val adder = List.fill(4)(_ => FullAdder1())
  for (adder)

@main def main = 
  FullAdderN(4).printCodeString
```

