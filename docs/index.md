---
hide:
  - navigation
---

# DFiant HDL (DFHDL) Docs

The Official DFiant Hardware Description Language (DFHDL) Documentation

---

![Build Status](https://github.com/DFiantHDL/DFiant/workflows/Build/badge.svg)
[![Discord Chat](https://img.shields.io/discord/721461308297576598.svg)](https://discord.gg/) 
[![Scala Steward badge](https://img.shields.io/badge/Scala_Steward-helping-blue.svg?style=flat&logo=data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAA4AAAAQCAMAAAARSr4IAAAAVFBMVEUAAACHjojlOy5NWlrKzcYRKjGFjIbp293YycuLa3pYY2LSqql4f3pCUFTgSjNodYRmcXUsPD/NTTbjRS+2jomhgnzNc223cGvZS0HaSD0XLjbaSjElhIr+AAAAAXRSTlMAQObYZgAAAHlJREFUCNdNyosOwyAIhWHAQS1Vt7a77/3fcxxdmv0xwmckutAR1nkm4ggbyEcg/wWmlGLDAA3oL50xi6fk5ffZ3E2E3QfZDCcCN2YtbEWZt+Drc6u6rlqv7Uk0LdKqqr5rk2UCRXOk0vmQKGfc94nOJyQjouF9H/wCc9gECEYfONoAAAAASUVORK5CYII=)](https://scala-steward.org)

Welcome to the DFiant hardware description language (DFHDL) documentation! 

DFHDL is a dataflow HDL and is embedded as a library in the [Scala programming language](https://www.scala-lang.org/){target="_blank"}. DFiant enables timing-agnostic and device-agnostic hardware description by using dataflow firing rules as logical constructs, coupled with modern software language features (e.g., inheritance, polymorphism, pattern matching) and classic HDL features (e.g., bit-accuracy, input/output ports). Additionally, DFHDL integrates two additional levels of hardware description abstractions: register-transfer (RT), which is equivalent to languages like Chisel and Amaranth; and event-driven (ED), which is equivalent to Verilog and VHDL. 

:octicons-arrow-right-24: [Get started][getting-started]

:octicons-arrow-right-24: [Read more about the technology][technology]

:octicons-arrow-right-24: [Run examples in your browser][run-in-browser]


## Documentation Status

We are actively working on a comprehensive user guide. We hope to be releasing it in the coming days.

In the meanwhile, checkout our [getting-started][getting-started] guide, to setup your system and try out a basic example.
Additionally, we placed several examples under the [Run In Browser][run-in-browser] section of the documentation, where you can try them right now.


## Required Knowledge

<u>You are ***not*** required to know Scala</u>, yet you are expected to understand basic object oriented concepts. This documentation attempts to bridge over any syntactic gaps you may arrive at. Nonetheless, as you attempt to create more complex and generic designs, more Scala knowledge will be required of you.

<u>You are ***not*** required to be an FPGA/ASIC expert</u>, yet you are expected to understand fundamental hardware description concepts found in languages such as Verilog and VHDL.  

<u>You ***are*** required to keep an open mind</u>. Some of these concepts may seem strange at first, but they were set after careful thought and planning. However, we are not infallible so feel free to [file an issue](https://github.com/DFiantHDL/DFiant/issues){target="_blank"} with questions and/or suggestions of different approaches we can take.

