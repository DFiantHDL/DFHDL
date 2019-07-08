---
typora-copy-images-to: ./
---

# DFiant: First Look

Your first encounter with the DFiant syntax, semantics and language features

---

In this section we provide simple examples to demonstrate various DFiant syntax, semantics and languages features. If you wish to understand how to run these examples yourself, please refer to the <u>Getting Started</u> chapter of this documentation. 

## Main Feature Overview

* Target and timing agnostic dataflow hardware description
* Strong bit-accurate type-safety
* Simplified port connections
* Automatic latency path balancing
* Automatic/manual pipelining
* Meta hardware description via rich Scala language constructs


## Basic Example: An Identity Function

Let's begin with a basic example. The dataflow design `ID` has a signed 16-bit input port `x` and a signed 16-bit output port `y`. We implemented an identity function between the input and output, meaning that for an input series $x_k$ the output series shall be $y_k=x_k$. Fig. 1a depicts a functional drawing of the design and Fig. 1b contains three tabs: the `ID.scala` DFiant compilation program code which implements `ID` and compiles it to VHDL (2008) and the generated VHDL files.

<p align="center">
  <img src="../first-look/id.png"><br>
  <b>Fig. 1a: Functional drawing of the dataflow design 'ID' with an input port 'x' and an output port 'y'</b><br>
</p>

``` scala tab="ID.scala"
import DFiant._ //Required in any DFiant compilation program

trait ID extends DFDesign { //This our `ID` dataflow design
  val x = DFSInt[16] <> IN  //The input port is a signed 16-bit integer
  val y = DFSInt[16] <> OUT	//The output port is a signed 16-bit integer
  y := x //Trivial direct input-to-output assignment
}

object IDApp extends DFApp.VHDLCompiler[ID] //The ID compilation program entry-point
```

``` vhdl tab="id.vhdl"
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.id_pkg.all;

entity id is
port (
  X                    : in  signed(15 downto 0);
  Y                    : out signed(15 downto 0)
);
end id;

architecture id_arch of id is
begin

async_proc : process (all)
begin
  Y                    <= X;
end process async_proc;

end id_arch;
```

``` vhdl tab="id_pkg.vhdl"
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package id_pkg is

function bit_reverse(s : std_logic_vector) return std_logic_vector;
         
function to_sl(b : boolean) return std_logic;
         
function to_sl(arg : std_logic_vector) return std_logic;
         
function to_slv(arg : std_logic) return std_logic_vector;
         
function to_slv(arg : unsigned) return std_logic_vector;
         
function to_slv(arg : signed) return std_logic_vector;
         

end package id_pkg;

package body id_pkg is

function bit_reverse(s : std_logic_vector) return std_logic_vector is
   variable v_s : std_logic_vector(s'high downto s'low);
begin
  for i in s'high downto s'low loop
    v_s(i) := s(s'high - i);
  end loop;
  return v_s;
end bit_reverse;
         
function to_sl(b : boolean) return std_logic is
begin
  if (b) then
    return '1';
  else
    return '0';
  end if;
end to_sl;
         
function to_sl(arg : std_logic_vector) return std_logic is
begin
  return arg(arg'low);
end to_sl;
         
function to_slv(arg : std_logic) return std_logic_vector is
begin
  if (arg = '1') then
    return "1";
  else
    return "0";
  end if;
end to_slv;
         
function to_slv(arg : unsigned) return std_logic_vector is
  variable slv : std_logic_vector(arg'length-1 downto 0);
begin
  slv := std_logic_vector(arg);
  return slv;
end to_slv;
         
function to_slv(arg : signed) return std_logic_vector is
  variable slv : std_logic_vector(arg'length-1 downto 0);
begin
  slv := std_logic_vector(arg);
  return slv;
end to_slv;
         
end package body id_pkg;
```

<p align="center">
  <b>Fig. 1b: A DFiant implementation of the identity function as a toplevel design and the generated VHDL files</b><br>
</p>

The Scala code in Fig. 1b describes a program that runs the DFiant compiler on an identity function dataflow design, `ID`. Since DFiant is a Scala library some if its compilation process is done statically via the Scala compiler and the rest during the Scala runtime execution. 

!!! summary "Writing a DFiant compilation program – easy as 1-2-3!"

	1. `#!scala import DFiant._` to import all the required namespace fields
	2. `#!scala trait _design_name_ extends DFDesign {}` to define your dataflow design. Populate your design with the required dataflow functionality.
	3. `#!scala object _program_name_ extends DFApp.VHDLCompiler[_design_name_]` to create your compilation program entry point.

??? info "ID.scala line-by-line breakdown"
	* **Line 1**: This `#!scala import` statement summons all the DFiant classes, types and objects into the current scope. This is a must in every DFiant codebase.
	* **Lines 3-7**: This `ID` Scala `#!scala trait` is extended from the `DFDesign` (abstract) class and therefore declares it as a dataflow design. The reason why this is a `#!scala trait` and not a `#!scala class` is discussed [later]() in this documentation. Currently, the *rule of thumb* to describe dataflow designs is to use traits that extend `DFDesign`.
		* **Lines 4-5**: Here we construct the input port `x` and output port `y`.  Both were set as a 16-bit signed integer dataflow variable via the `DFSInt[W]` constructor, where `W` is a width ***type*** argument that can accept any positive integer literal. It is also possible to use a width ***term*** argument via`DFSInt(width)`. DFiant also support various types such as `DFBits`, `DFUInt`, and `DFBool`. All these dataflow variable construction options and more are discussed [later](/getting-started/) in this documentation. <br />The syntax `#!scala val _name_ = _dataflow_variable_constructor_ <> _direction_` is used to construct a port and give it a named Scala reference. The Scala reference name will affect the name of this port when compiled to the required backend representation. 
		* **Line 6**: The assignment operator `:=` set the dataflow output port to receive input port values as they are.
	* **Line 9**: This object is an extension of a [Scala `App` trait](https://www.scala-lang.org/api/current/scala/App.html) that creates a `main` entry point for the DFiant compilation program. By inheriting `DFApp.VHDLCompiler[_top_]` we also generate the top design and execute the compilation and commitment to VHDL files. 

??? info "Generated VHDL files observations"
	* The id.vhdl file is readable and maintains the names set in the DFiant design. The generated files follow various writing conventions such as capitalized port names and proper code alignment.
	* The id_pkg.vhdl is a package file that is shared between all VHDL files generated by DFiant and  contains common conversion functions that may be required. Additionally it may contain other definitions like enumeration types.

---

## Hierarchy and Connection Example

![idtop](idtop.png)



``` scala tab="IDTop.scala" hl_lines="6"
import DFiant._ //Required in any DFiant compilation program

trait ID extends DFDesign { //This our `ID` dataflow design
  val x = DFSInt[16] <> IN  //The input port is a signed 16-bit integer
  val y = DFSInt[16] <> OUT	//The output port is a signed 16-bit integer
  y <> x //Trivial direct input-to-output connection
}

trait IDTop extends DFDesign { //This our `IDTop` dataflow design
  val x = DFSInt[16] <> IN  //The input port is a signed 16-bit integer
  val y = DFSInt[16] <> OUT	//The output port is a signed 16-bit integer
  val id1 = new ID {} //First instance of the `ID` design
  val id2 = new ID {} //Second instance of the `ID` design
  id1.x <> x      //Connecting parent input port to child input port
  id1.y <> id2.x  //Connecting sibling instance ports
  id2.y <> y      //Connecting parent output port to child output port
}

object IDTopApp extends DFApp.VHDLCompiler[IDTop] //The IDTop compilation program entry-point
```

``` vhdl tab="idtop.vhdl"
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.idtop_pkg.all;

entity idtop is
port (
  X                    : in  signed(15 downto 0);
  Y                    : out signed(15 downto 0)
);
end idtop;

architecture idtop_arch of idtop is
  signal id1_X         : signed(15 downto 0);
  signal id1_Y         : signed(15 downto 0);
  signal id2_X         : signed(15 downto 0);
  signal id2_Y         : signed(15 downto 0);
begin

id1 : entity work.id(id_arch) port map (
  X                    => id1_X,
  Y                    => id1_Y
);

id2 : entity work.id(id_arch) port map (
  X                    => id2_X,
  Y                    => id2_Y
);

async_proc : process (all)
begin
  id1_X                <= X;
  id2_X                <= id1_Y;
  Y                    <= id2_Y;
end process async_proc;

end idtop_arch;
```

``` vhdl tab="id.vhdl"
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.id_pkg.all;

entity id is
port (
  X                    : in  signed(15 downto 0);
  Y                    : out signed(15 downto 0)
);
end id;

architecture id_arch of id is
begin

async_proc : process (all)
begin
  Y                    <= X;
end process async_proc;

end id_arch;
```

``` vhdl tab="idtop_pkg.vhdl"
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package idtop_pkg is

function bit_reverse(s : std_logic_vector) return std_logic_vector;
         
function to_sl(b : boolean) return std_logic;
         
function to_sl(arg : std_logic_vector) return std_logic;
         
function to_slv(arg : std_logic) return std_logic_vector;
         
function to_slv(arg : unsigned) return std_logic_vector;
         
function to_slv(arg : signed) return std_logic_vector;
         

end package idtop_pkg;

package body idtop_pkg is

function bit_reverse(s : std_logic_vector) return std_logic_vector is
   variable v_s : std_logic_vector(s'high downto s'low);
begin
  for i in s'high downto s'low loop
    v_s(i) := s(s'high - i);
  end loop;
  return v_s;
end bit_reverse;
         
function to_sl(b : boolean) return std_logic is
begin
  if (b) then
    return '1';
  else
    return '0';
  end if;
end to_sl;
         
function to_sl(arg : std_logic_vector) return std_logic is
begin
  return arg(arg'low);
end to_sl;
         
function to_slv(arg : std_logic) return std_logic_vector is
begin
  if (arg = '1') then
    return "1";
  else
    return "0";
  end if;
end to_slv;
         
function to_slv(arg : unsigned) return std_logic_vector is
  variable slv : std_logic_vector(arg'length-1 downto 0);
begin
  slv := std_logic_vector(arg);
  return slv;
end to_slv;
         
function to_slv(arg : signed) return std_logic_vector is
  variable slv : std_logic_vector(arg'length-1 downto 0);
begin
  slv := std_logic_vector(arg);
  return slv;
end to_slv;
         
end package body idtop_pkg;
```



## Finite State Machine (FSM) Example

``` scala tab="FSM.scala"
import DFiant._ //Required in any DFiant compilation program

object SeqState extends Enum.Auto { //State entry enumeration
  val S0, S1, S10, S100, S1001 = Entry
}

trait FSM extends DFDesign { //This our `FSM` dataflow design
  val x = DFBool() <> IN  //The input port is a boolean
  val y = DFBool() <> OUT //The output port is a boolean
  val ss = DFEnum(SeqState) init SeqState.S0 //The sequence state

  matchdf(ss) //dataflow match on the state
    .casedf(SeqState.S0) {
      y := 0
      ifdf (x == 0) {
        ss := SeqState.S0
      }.elsedf {
        ss := SeqState.S1
      }
    }.casedf(SeqState.S1) {
      y := 0
      ifdf (x == 0) {
        ss := SeqState.S10
      }.elsedf {
        ss := SeqState.S1
      }
    }.casedf(SeqState.S10) {
      y := 0
      ifdf (x == 0) {
        ss := SeqState.S100
      }.elsedf {
        ss := SeqState.S1
      }
    }.casedf(SeqState.S100) {
      y := 0
      ifdf (x == 0) {
        ss := SeqState.S0
      }.elsedf {
        ss := SeqState.S1001
      }
    }.casedf(SeqState.S1001) {
      y := 1
      ifdf (x == 0) {
        ss := SeqState.S10
      }.elsedf {
        ss := SeqState.S1
      }
    }
}

object FSMApp extends DFApp.VHDLCompiler[FSM] //The FSM compilation program entry-point
```

``` vhdl tab="fsm.vhdl"
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.fsm_pkg.all;

entity fsm is
port (
  CLK                  : in  std_logic;
  RSTn                 : in  std_logic;
  X                    : in  std_logic;
  Y                    : out std_logic
);
end fsm;

architecture fsm_arch of fsm is
  signal ss            : SeqState_type;
  signal ss_prev1      : SeqState_type;
begin

sync_proc : process (CLK, RSTn)
begin
  if RSTn = '0' then
    ss_prev1           <= E_SEQSTATE_S0;
  elsif rising_edge(CLK) then
    ss_prev1           <= ss;
  end if;
end process sync_proc;

async_proc : process (all)
  variable v_Y         : std_logic;
  variable v_ss        : SeqState_type;
begin
  v_ss                 := ss_prev1;
  case v_ss is
    when E_SEQSTATE_S0 =>
      v_Y              := '0';
      if X = '0' then
        v_ss           := E_SEQSTATE_S0;
      else
        v_ss           := E_SEQSTATE_S1;
      end if;
    when E_SEQSTATE_S1 =>
      v_Y              := '0';
      if X = '0' then
        v_ss           := E_SEQSTATE_S10;
      else
        v_ss           := E_SEQSTATE_S1;
      end if;
    when E_SEQSTATE_S10 =>
      v_Y              := '0';
      if X = '0' then
        v_ss           := E_SEQSTATE_S100;
      else
        v_ss           := E_SEQSTATE_S1;
      end if;
    when E_SEQSTATE_S100 =>
      v_Y              := '0';
      if X = '0' then
        v_ss           := E_SEQSTATE_S0;
      else
        v_ss           := E_SEQSTATE_S1001;
      end if;
    when E_SEQSTATE_S1001 =>
      v_Y              := '1';
      if X = '0' then
        v_ss           := E_SEQSTATE_S10;
      else
        v_ss           := E_SEQSTATE_S1;
      end if;
    when others =>
  end case;
  Y                    <= v_Y;
  ss                   <= v_ss;
end process async_proc;

end fsm_arch;
```

``` vhdl tab="fsm_pkg.vhdl" hl_lines="19"
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package fsm_pkg is

function bit_reverse(s : std_logic_vector) return std_logic_vector;
         
function to_sl(b : boolean) return std_logic;
         
function to_sl(arg : std_logic_vector) return std_logic;
         
function to_slv(arg : std_logic) return std_logic_vector;
         
function to_slv(arg : unsigned) return std_logic_vector;
         
function to_slv(arg : signed) return std_logic_vector;
         
type SeqState_type is (E_SEQSTATE_S0, E_SEQSTATE_S1, E_SEQSTATE_S10, E_SEQSTATE_S100, E_SEQSTATE_S1001);
end package fsm_pkg;

package body fsm_pkg is

function bit_reverse(s : std_logic_vector) return std_logic_vector is
   variable v_s : std_logic_vector(s'high downto s'low);
begin
  for i in s'high downto s'low loop
    v_s(i) := s(s'high - i);
  end loop;
  return v_s;
end bit_reverse;
         
function to_sl(b : boolean) return std_logic is
begin
  if (b) then
    return '1';
  else
    return '0';
  end if;
end to_sl;
         
function to_sl(arg : std_logic_vector) return std_logic is
begin
  return arg(arg'low);
end to_sl;
         
function to_slv(arg : std_logic) return std_logic_vector is
begin
  if (arg = '1') then
    return "1";
  else
    return "0";
  end if;
end to_slv;
         
function to_slv(arg : unsigned) return std_logic_vector is
  variable slv : std_logic_vector(arg'length-1 downto 0);
begin
  slv := std_logic_vector(arg);
  return slv;
end to_slv;
         
function to_slv(arg : signed) return std_logic_vector is
  variable slv : std_logic_vector(arg'length-1 downto 0);
begin
  slv := std_logic_vector(arg);
  return slv;
end to_slv;
         
end package body fsm_pkg;
```

---

## Simple Moving Average

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

We begin with a [simple moving average](https://en.wikipedia.org/wiki/Moving_average) (SMA) example. In this example, the signed 16-bit  input,

$ a_0 = 0;  a_k = a_{k-1} - x_{k-4}+x_k; y_k = a_k/4$





something

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

