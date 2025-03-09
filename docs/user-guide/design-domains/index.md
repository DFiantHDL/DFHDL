# Design Domains

DFHDL offers three key domain abstractions—dataflow (DF), register-transfer (RT), and event-driven (ED)—all within a single HDL, as illustrated in the following figure. This unique capability allows developers to employ a cohesive syntax to seamlessly blend these abstractions: DF, RT, and ED. Each abstraction brings its own set of advantages in terms of control, synthesizability, simulation speed, and functional correctness.

The RT abstraction mirrors the capabilities found in languages like Chisel and Amaranth, while the ED abstraction aligns with the functionalities of VHDL and Verilog. Through an intelligent compilation process, the DFHDL compiler transitions from the higher-level DF abstraction through RT and ultimately to ED. The choice of compilation dialect—whether VHDL 93/2008 or Verilog/SystemVerilog—determines the final ED code representation.

![design-domains](design-domains-light.png#only-light)
![design-domains](design-domains-dark.png#only-dark)

## Dataflow (DF) Domain

The dataflow domain provides the highest level of abstraction, focusing on data dependencies rather than timing.

### Key Features
- Timing-agnostic descriptions
- Implicit state handling
- Token stream semantics
- History access via `.prev`

### Example
```scala
class Accumulator extends DFDesign:
  val input = UInt(8) <> IN
  val sum = UInt(16) <> OUT init 0
  sum := sum.prev + input  // Implicit state handling
```

## Register Transfer (RT) Domain

The RT domain provides explicit control over registers and timing while maintaining hardware-friendly abstractions.

### Domain Configuration

#### Clock Configuration
```scala
// Clock edge options
val risingEdge = ClkCfg.Edge.Rising   // Rising edge triggered
val fallingEdge = ClkCfg.Edge.Falling // Falling edge triggered

// Clock configuration with options
val clkCfg = ClkCfg(
  edge = ClkCfg.Edge.Rising,    // Clock edge sensitivity
  rate = 50.MHz,                // Clock frequency
  portName = "clk",             // Port name in generated code
  inclusionPolicy = ClkCfg.InclusionPolicy.AsNeeded // Port inclusion policy
)
```

#### Reset Configuration
```scala
// Reset mode options
val asyncRst = RstCfg.Mode.Async  // Asynchronous reset
val syncRst = RstCfg.Mode.Sync    // Synchronous reset

// Reset active level
val activeHigh = RstCfg.Active.High // Reset active at 1
val activeLow = RstCfg.Active.Low   // Reset active at 0

// Reset configuration with options
val rstCfg = RstCfg(
  mode = RstCfg.Mode.Sync,         // Reset mode
  active = RstCfg.Active.High,     // Active level
  portName = "rst",                // Port name in generated code
  inclusionPolicy = RstCfg.InclusionPolicy.AsNeeded // Port inclusion policy
)
```

#### RTDomain Configuration
```scala
// Complete domain configuration
val domainCfg = RTDomainCfg(clkCfg, rstCfg)

// Special configurations
val combDomain = RTDomainCfg.Comb           // Combinational domain (no clk/rst)
val defaultDomain = RTDomainCfg.Default     // Default configuration
val derivedDomain = RTDomainCfg.Derived     // Derived from parent domain

// Domain without reset
val noRstDomain = domainCfg.norst           // Remove reset from configuration
```

#### Inclusion Policies
The `InclusionPolicy` determines how clock and reset ports are included in the generated code:

- `AsNeeded`: Only includes ports when they are actually used
- `AlwaysAtTop`: Always includes ports at the top level with `@unused` annotation if not used

### Domain Types

#### Basic RT Domain
```scala
class BasicRTDesign extends RTDesign(domainCfg):
  val x = UInt(8) <> IN
  val y = UInt(8) <> OUT.REG init 0
  y := x.reg  // Registered on configured clock edge
```

#### Multiple Clock Domains
```scala
class MultiClockDesign extends RTDesign(mainCfg):
  // Main domain at 100MHz
  val mainClk = ClkCfg(edge = ClkCfg.Edge.Rising, rate = 100.MHz)
  val mainRst = RstCfg(mode = RstCfg.Mode.Sync, active = RstCfg.Active.High)
  
  // Slow domain at 25MHz
  val slowClk = ClkCfg(edge = ClkCfg.Edge.Rising, rate = 25.MHz)
  val slowDomain = new RTDomain(RTDomainCfg(slowClk, mainRst)):
    val slow_reg = UInt(8) <> VAR.REG init 0
    
  // Fast domain at 200MHz
  val fastClk = ClkCfg(edge = ClkCfg.Edge.Rising, rate = 200.MHz)
  val fastDomain = new RTDomain(RTDomainCfg(fastClk, mainRst)):
    val fast_reg = UInt(8) <> VAR.REG init 0
```

#### Related Domains
```scala
class RelatedDomainsDesign extends RTDesign(mainCfg):
  val baseDomain = new RTDomain(baseCfg):
    val base_reg = UInt(8) <> VAR.REG init 0
    
  // Inherits clock/reset from baseDomain
  val relatedDomain = new baseDomain.RelatedDomain:
    val related_reg = UInt(8) <> VAR.REG init 0
    
  // Related domain without reset
  val noRstRelated = new baseDomain.RelatedDomain(baseCfg.norst):
    val norst_reg = UInt(8) <> VAR.REG init 0
```

### Register Types and Initialization

#### Register Declarations vs Aliases
```scala
class RegisterPatterns extends RTDesign(cfg):
  val x = UInt(8) <> IN
  
  // Register Declaration - creates a new register
  val reg1 = UInt(8) <> VAR.REG init 0  // Variable register
  val out1 = UInt(8) <> OUT.REG init 0  // Output register
  
  // Register Alias - creates a registered version of a signal
  val delayed = x.reg      // One cycle delay of x
  val delayed2 = x.reg(2)  // Two cycle delay of x
```

#### Register Access Patterns
```scala
class RegisterAccess extends RTDesign(cfg):
  val x = UInt(8) <> IN
  val reg = UInt(8) <> VAR.REG init 0
  val out = UInt(8) <> OUT.REG init 0
  
  // CORRECT: Writing to register input using .din
  reg.din := x            // Updates register input
  out.din := reg         // Updates output register input
  
  // INCORRECT: Attempting to write to register output
  reg := x               // Error: Can't write to register output
  out := reg            // Error: Can't write to register output
  
  // Reading from registers (always reads output)
  val value = reg        // Reads register output
  val outValue = out     // Reads output register value
```

#### Register Composition
```scala
class RegisterComposition extends RTDesign(cfg):
  val x = UInt(8) <> IN
  
  // Using register declarations
  val reg1 = UInt(8) <> VAR.REG init 0
  val reg2 = UInt(8) <> VAR.REG init 0
  reg1.din := x
  reg2.din := reg1      // Chaining registers
  
  // Using register aliases
  val stage1 = x.reg    // Same as reg1
  val stage2 = x.reg(2) // Same as reg2, but more concise
  
  // Mixing declarations and aliases
  val reg3 = UInt(8) <> VAR.REG init 0
  reg3.din := stage2    // Can mix both styles
```

#### Advanced Register Patterns
```scala
class AdvancedRegisters extends RTDesign(cfg):
  val x = UInt(8) <> IN
  val y = UInt(8) <> IN
  
  // Conditional registration
  val reg = UInt(8) <> VAR.REG init 0
  if (x > 10)
    reg.din := y     // Register y when x > 10
  else
    reg.din := x     // Register x otherwise
    
  // Register with enable
  val enReg = UInt(8) <> VAR.REG init 0
  val en = Bit <> IN
  if (en)
    enReg.din := x   // Only update when enabled
    
  // Multiple cycle delays with processing
  val processed = (x + 1).reg(2)  // Add 1 and delay 2 cycles
```

#### Common Patterns and Pitfalls
```scala
class RegisterPitfalls extends RTDesign(cfg):
  val x = UInt(8) <> IN
  val reg = UInt(8) <> VAR.REG init 0
  
  // GOOD: Explicit input/output separation
  reg.din := x + 1        // Write to input
  val result = reg + 2    // Read from output
  
  // BAD: Attempting to write to output
  reg := x + 1           // Error: Writing to output
  
  // GOOD: Register alias for simple delays
  val delayed = x.reg    // Clean and clear intent
  
  // BAD: Unnecessary register declaration for simple delay
  val regDelay = UInt(8) <> VAR.REG init 0
  regDelay.din := x      // More verbose than needed
```

## Event-Driven (ED) Domain

The ED domain provides the lowest level of abstraction, with explicit process blocks and event sensitivity.

### Process Types
```scala
class EDExample extends EDDesign:
  val clk = Bit <> IN
  val rst = Bit <> IN
  val x = UInt(8) <> IN
  val y = UInt(8) <> OUT

  // Combinational process
  process(all):
    y := x + 1

  // Clock-sensitive process
  process(clk.rising):
    y := x

  // Clock and reset process
  process(clk, rst):
    if (rst)
      y := 0
    else if (clk.rising)
      y := x
```

### Assignment Types
- Blocking (`:=`): Immediate effect
- Non-blocking (`:==`): Scheduled update
```scala
process(clk.rising):
  val temp = x  // Blocking read
  y :== temp    // Non-blocking write
```

## Domain Interaction

### Cross-Domain Communication
```scala
class CrossDomainExample extends RTDesign(mainCfg):
  val x = UInt(8) <> IN
  
  val domainA = new RTDomain(cfgA):
    val reg_a = UInt(8) <> VAR.REG init 0
    reg_a := x.reg
    
  val domainB = new RTDomain(cfgB):
    val reg_b = UInt(8) <> VAR.REG init 0
    reg_b := domainA.reg_a.reg  // Cross-domain registration
```

### Domain Flattening
During compilation, nested domains are flattened while preserving clock and reset relationships:
```scala
// Original nested domains
val innerDomain = new RTDomain(innerCfg):
  val reg = UInt(8) <> VAR.REG init 0

// After flattening
val innerDomain_reg = UInt(8) <> VAR.REG init 0
process(innerDomain_clk.rising):
  if (innerDomain_rst) 
    innerDomain_reg := 0
  else 
    innerDomain_reg := next_value
```

## Compilation Flow

1. **Domain Resolution**:
   - Flattens nested domains
   - Resolves clock and reset configurations
   - Establishes domain hierarchies

2. **State Management**:
   - Converts DF `.prev` to explicit registers
   - Handles RT register declarations
   - Manages ED process state variables

3. **Process Generation**:
   - Converts DF and RT to ED processes
   - Optimizes sensitivity lists
   - Handles blocking/non-blocking assignments

4. **Backend Generation**:
   - Generates VHDL or Verilog code
   - Preserves timing relationships
   - Maintains design hierarchy

## Best Practices

1. **Domain Selection**:
   - Use DF for algorithmic descriptions
   - Use RT for timing-critical paths
   - Use ED for low-level control

2. **Clock Domain Crossing**:
   - Use explicit synchronization
   - Maintain clear domain boundaries
   - Document clock relationships

3. **State Management**:
   - Initialize all registers
   - Use appropriate reset strategies
   - Consider reset domains

4. **Performance Optimization**:
   - Balance domain abstractions
   - Use appropriate clock domains
   - Consider resource utilization
