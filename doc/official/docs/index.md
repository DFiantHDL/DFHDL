# DFiant HDL Docs

The Official DFiant Hardware Description Language (HDL) Documentation

---

[![Build Status](https://travis-ci.com/soronpo/DFiant.svg?token=dzwzuUsZuyhzAjyvw87v&branch=master)](https://travis-ci.com/soronpo/DFiant)

Welcome to the DFiant hardware description language (HDL) documentation! 

DFiant is a dataflow HDL and is embedded as a library in the [Scala programming language](https://www.scala-lang.org/). DFiant enables  timing-agnostic and device-agnostic hardware description by using dataflow firing rules as logical constructs, coupled with modern software language features (e.g., inheritance, polymorphism) and classic HDL features (e.g., bit-accuracy, input/output ports).

By this point you may already have some questions: 

* Why do we need yet another HDL? 
* Why are high-level synthesis (HLS) tools not enough?

* What is a dataflow HDL? 

Answers to these questions await you at our [Introduction][Why] section.



## Required Knowledge

<u>You are ***not*** required to know Scala</u>, yet you are expected to understand basic object oriented concepts. This documentation attempts to bridge over any syntactic gaps you may arrive at. Nonetheless, as you attempt to create more complex and generic designs, more Scala knowledge will be required of you.

<u>You are ***not*** required to be an FPGA/ASIC expert</u>, yet you are expected to understand fundamental hardware description concepts found in languages such as Verilog and VHDL.  

<u>You ***are*** required to keep an open mind</u>. Some of these concepts may seem strange at first, but they were set after careful thought and planning. However, we are not infallible so feel free to [file an issue](https://github.com/DFiantHDL/DFiant/issues) and even fix it yourself and [submit a PR](https://github.com/DFiantHDL/DFiant/pulls) ;).



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


$$
y_k=\left(x_k+x_{k-1}+x_{k-2}+x_{k-3}\right)/4
$$


We begin with a [simple moving average](https://en.wikipedia.org/wiki/Moving_average) (SMA) example. In this example, the signed 16-bit  input, $x$ 

 $y_k=\left(x_k+x_{k-1}+x_{k-2}+x_{k-3}\right)/4$

```scala
trait SimpleMovingAverage extends DFDesign {
  val x   = DFSInt[16] <> IN  init 0 		//The signed 16-bit integer input stream
  val y   = DFSInt[16] <> OUT						//The signed 16-bit integer output stream
  val acc = DFSInt[18] init 0						//The signed 18-bit accumulator state
  acc := acc - x.prev(4) + x						//Accumulation functionality construction
  y := (acc / 4).toWidth(16)
}
```


$$
y_k=\left(x_k+x_{k-1}+x_{k-2}+x_{k-3}\right)/4
$$



## Looks cool! I wish to know more



## First release and more info coming soon...



## References

[Scala programming language]: https://www.scala-lang.org/

