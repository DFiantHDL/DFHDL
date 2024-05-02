---
hide:
  - toc
---

# 1-Bit Full Adder

```scastie 
import dfhdl.*

class FullAdder1 extends EDDesign:
  val a, b, c_in = Bit <> IN
  val sum, c_out = Bit <> OUT

  sum <> (a ^ b ^ c_in)
  c_out <> (a && b || b && c_in || c_in && a)

@main def main = 
  FullAdder1().printCodeString
```

