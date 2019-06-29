# DFiant: First Look

&nbsp;

---

## Feature Overview



## Basic Example

Let's begin with a basic example. In this example, the signed 16-bit  input, $x$ 

 $y_k=x_k$

```scala
import DFiant._ //DFiant is a Scala library. This import statement summons all the 
                //DFiant classes, types and objects into the current scope.

//This basic Scala trait is extended from a DFDesign class and therefore
//it is a dataflow design
//The reason why this is a trait and not a class is given later on 
trait Basic extends DFDesign {
  val x   = DFSInt[16] <> IN  //The input signed 16-bit integer stream
  val y   = DFSInt[16] <> OUT						//The output 16-bit singed
  y := x
}

object BasicApp extends App {
  val bsc = new Basic {} //Instantiate Basic as a top-level entity
  bsc.compileToVHDL.toFile("bsc.vhdl") //Compile to a single VHDL file.
}
```



We begin with a [simple moving average](https://en.wikipedia.org/wiki/Moving_average) (SMA) example. In this example, the signed 16-bit  input, $x$ 

 $y_k=\left(x_k+x_{k-1}+x_{k-2}+x_{k-3}\right)/4$

```scala
trait SimpleMovingAverage extends DFDesign {
  val x   = DFSInt[16] <> IN  init 0 		//The signed 16-bit integer input stream
  val y   = DFSInt[16] <> OUT						//The signed 16-bit integer output stream
  val sum = ((x + x.prev).wc + (x.prev(2) + x.prev(3)).wc).wc
  y := (sum / 4).toWidth(16)
}
```

We begin with a [simple moving average](https://en.wikipedia.org/wiki/Moving_average) (SMA) example. In this example, the signed 16-bit  input, $x$ 

$ a_0 = 0 $
$ a_k = a_{k-1} - x_{k-4}+x_k $
$ y_k = a_k/4$

```scala
trait SimpleMovingAverage extends DFDesign {
  val x   = DFSInt[16] <> IN  init 0 		//The signed 16-bit integer input stream
  val y   = DFSInt[16] <> OUT						//The signed 16-bit integer output stream
  val acc = DFSInt[18] init 0						//The signed 18-bit accumulator state
  acc := acc - x.prev(4) + x						//Accumulation functionality construction
  y := (acc / 4).toWidth(16)
}
```



## Looks cool! I wish to know more

