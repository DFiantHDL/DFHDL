---
hide:
  - toc
---

# Counter

```scastie 
import dfhdl.*

class Counter(val width: Int <> CONST) extends RTDesign:
  val cnt = UInt(width) <> OUT init 0
  cnt := cnt.reg + 1

@main def main = 
  Counter(8).printCodeString
```

