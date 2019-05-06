# DFiant HDL Docs

The Official DFiant Hardware Description Language (HDL) Documentation

---

[![Build Status](https://travis-ci.com/soronpo/DFiant.svg?token=dzwzuUsZuyhzAjyvw87v&branch=master)](https://travis-ci.com/soronpo/DFiant)



```scala
import DFiant._ 

trait SlidingAverage extends DFDesign {
  val i   = DFSInt[16] <> IN  init 0
  val o   = DFSInt[16] <> OUT
  val acc = DFSInt[18] init 0
  acc := acc - i.prev(4) + i
  o := (acc / 4).toWidth(16)
}
```





## First release and info coming soon...




