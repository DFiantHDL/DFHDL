# The DFiant Technology {#technology}

DFiant navigates the complexities of hardware design with a nuanced approach that addresses the limitations of traditional methods. Register-transfer level (RTL) abstraction, while precise, often shifts the design focus from the "what" of functionality to the "how" of implementation. This subtle yet significant distinction restricts design flexibility, making it challenging to optimize for external constraints like performance or power without compromising on tool automation capabilities. The detailed nature of RTL leads to complex, verbose coding, and extended simulation time. Although such granularity of control is sometimes necessary, it often exceeds the requirements of many applications.

Conversely, High-Level Synthesis (HLS) tools offer an abstraction that simplifies hardware design, particularly for those without a background in hardware engineering. By utilizing familiar programming languages and incorporating auto-pipelining and optimization, HLS makes hardware acceleration more accessible. However, the inherent sequential semantics of these languages can obstruct the development of parallel hardware architectures, making it especially challenging to describe processors or synchronous interfaces.

A newer category, High-Level RTLs (HL-RTLs), such as [Chisel](https://www.chisel-lang.org/) and [Amaranth](https://amaranth-lang.org/docs/amaranth/latest/), aims to bridge these gaps. Embedded within popular programming languages as domain-specific languages (DSLs), these HL-RTLs introduce innovative constructs for hardware generation. However, despite these significant advancements, they remain anchored to the RTL model, which can restrict implementation flexibility. Furthermore, their level of abstraction does not fully support the verification capabilities found in VHDL and Verilog, which benefit from event-driven semantics.

DFiant attempts to introduce an optimal middle ground, which covers key HDL technologies across dimensions of both control and productivity (sort of "have the cake and it it too").


## The Dataflow Hardware Description Abstraction

At the heart of DFiant's innovation is the dataflow hardware description abstraction, a paradigm shift from the traditional RTL model. Instead of relying on wires and registers, dataflow abstraction employs streams of data tokens. This fundamental difference explains why RTL is inherently tied to device specifications and timing constraints, whereas dataflow abstraction remains neutral to such parameters.

<style>
@media (min-width: 768px) {  
  article.md-content__inner.md-typeset p {
    img {
      max-width: 70%;
    }
  }

  .md-content__inner > p:nth-child(9) {
    text-align: center;
  }

  .md-content__inner > p:nth-child(14) {
    text-align: center;
  }
}
</style>

![dataflow-abstraction](dataflow-light.png#only-light)
![dataflow-abstraction](dataflow-dark.png#only-dark)

In RTL design, designers are tasked with specifying the exact operations that occur in each clock cycle, closely aligning the design with the physical timing of the hardware. Dataflow abstraction, on the other hand, focuses on the sequence of operations based on data dependencies, without mandating when each operation should start or finish within the clock cycles. This distinction allows operations in the dataflow model to be free from the rigid timing of clock cycles, granting compilers in the toolchain greater flexibility to schedule operations and effectively pipeline the design.

Moreover, RTL design often requires the strategic placement of registers for various functions, anchoring the design to specific timing requirements. We categorize register use in RTL into three primary functions: backend synchronization, interface synchronization, and intrinsic design functionality, such as state management. The figure above illustrates the diverse applications of registers within RTL designs. Unlike RTL, where registers are uniformly represented, dataflow abstraction introduces unique language constructs or constraints for each register type. This differentiation enables the DFHDL compiler to distinguish between registers integral to the function and those necessary for implementation, providing subsequent design stages valuable insights for optimization. In contrast, RTL often relies on comments within the code to convey this information, if it is documented at all.

## Three Design Domain Abstractions within a Single HDL

DFHDL stands out by offering a comprehensive integration of three key domain abstractions—dataflow (DF), register-transfer (RT), and event-driven (ED)—all within a single HDL, as illustrated in Figure 4. This unique capability allows developers to employ a cohesive syntax to seamlessly blend these abstractions: DF, RT, and ED. Each abstraction brings its own set of advantages in terms of control, synthesizability, simulation speed, and functional correctness.

![design-domains](design-domains-light.png#only-light)
![design-domains](design-domains-dark.png#only-dark)

The RT abstraction mirrors the capabilities found in languages like Chisel and Amaranth, while the ED abstraction aligns with the functionalities of VHDL and Verilog. Through an carefully constructed compilation process, the DFHDL compiler transitions from the higher-level DF abstraction through RT and ultimately to ED. The choice of compilation dialect—whether VHDL 93/2008 or Verilog/SystemVerilog—determines the final ED code representation.

A standout feature of DFHDL is its ability to regenerate the code in DFHDL syntax at any point in the compilation process, including intermediate stages. This transparency offers developers valuable insights into each step of the optimization or compilation, a stark contrast to many HLS tools that produce hard-to-interpret code, leaving developers to decipher the underlying processes in case of issues.


## More Details

For more details on motivation, you can read [this position paper](https://capra.cs.cornell.edu/latte21/paper/4.pdf).
