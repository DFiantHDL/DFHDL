[![Build Status](https://travis-ci.com/soronpo/DFiant.svg?token=dzwzuUsZuyhzAjyvw87v&branch=master)](https://travis-ci.com/soronpo/DFiant)

# Welcome to the DFiant hardware fescription language (HDL) documentation!

```scala
import DFiant._ 

trait SlidingAverage extends DFDesign {
  val inlet  = DFSInt(16) <> IN  init 0
  val outlet = DFSInt(16) <> OUT
  val acc    = DFSInt(18) init 0
  acc := acc - i.prev(4) + i
}
```

## Commands





## Project layout

    mkdocs.yml    # The configuration file.
    docs/
        index.md  # The documentation homepage.
        ...       # Other markdown pages, images and other files.
