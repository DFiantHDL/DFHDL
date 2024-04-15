# Design Domains

DFHDL offers three key domain abstractions—dataflow (DF), register-transfer (RT), and event-driven (ED)—all within a single HDL, as illustrated in the following figure. This unique capability allows developers to employ a cohesive syntax to seamlessly blend these abstractions: DF, RT, and ED. Each abstraction brings its own set of advantages in terms of control, synthesizability, simulation speed, and functional correctness.

The RT abstraction mirrors the capabilities found in languages like Chisel and Amaranth, while the ED abstraction aligns with the functionalities of VHDL and Verilog. Through an intelligent compilation process, the DFHDL compiler transitions from the higher-level DF abstraction through RT and ultimately to ED. The choice of compilation dialect—whether VHDL 93/2008 or Verilog/SystemVerilog—determines the final ED code representation.

![design-domains](design-domains-light.png#only-light)
![design-domains](design-domains-dark.png#only-dark)
