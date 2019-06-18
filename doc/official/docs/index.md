# DFiant HDL Docs

The Official DFiant Hardware Description Language (HDL) Documentation

---

[![Build Status](https://travis-ci.com/soronpo/DFiant.svg?token=dzwzuUsZuyhzAjyvw87v&branch=master)](https://travis-ci.com/soronpo/DFiant)

Welcome to the DFiant hardware description language (HDL) documentation! 

DFiant is a dataflow HDL and is embedded as a library in the [Scala programming language](https://www.scala-lang.org/). DFiant enables  timing-agnostic and device-agnostic hardware description by using dataflow firing rules as a logical construct, coupled with modern software language features (e.g., inheritance, polymorphism) and classic HDL features (e.g., bit-accuracy, input/output ports).

By this point you may already have some questions: 

* Why do we need yet another HDL? 
* Why are high-level synthesis (HLS) tools not enough?

* What is a dataflow HDL? 

Answers to these questions await you at section [Why...?][Why]



## Required Knowledge

You are *<u>not</u>* required to know Scala, yet we do expect you to know basic object oriented concepts.

You are *<u>not</u>* required to be an FPGA/ASIC expert, yet we do expect you to understand general hardware description concepts found in languages such as Verilog and VHDL.

You *<u>are</u>* required to have an open mind. Some of these concepts may seem strange at first, but 



## Basic Example

Let's begin with a basic example. In this example, the signed 16-bit  input, $x$ 

 $y_k=x_k$

```scala
import DFiant._ //

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
import DFiant._ 

trait SimpleMovingAverage extends DFDesign {
  val x   = DFSInt[16] <> IN  init 0 		//The signed 16-bit integer input stream
  val y   = DFSInt[16] <> OUT						//The signed 16-bit integer output stream
  val acc = DFSInt[18] init 0						//The signed 18-bit accumulator state
  acc := acc - x.prev(4) + x						//Accumulation functionality construction
  y := (acc / 4).toWidth(16)
}

object SMAApp extends App {
  val sma = new SimpleMovingAverage {} //Instantiate SMA as a top-level entity
  sma.compileToVHDL.toFile("sma.vhdl") //Compiler to a VHDL Code.
}
```


$$
y_k=\left(x_k+x_{k-1}+x_{k-2}+x_{k-3}\right)/4
$$



## Looks cool! I wish to know more



## First release and more info coming soon...



## References

[Scala programming language]: https://www.scala-lang.org/

