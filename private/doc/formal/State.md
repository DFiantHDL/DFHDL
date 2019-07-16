# State & Initialization

Semantically, every DFiant dataflow variable references a token stream (TS). 



Formalism Brainstorming:

* Unless stated otherwise, all dataflow variables are always consuming and always producing.

* Previous token initialization:

  * The token history stream can be initialized.

  * Initialization does not mutate the dataflow variable.

  * Initialization has no effect on the TS. Only when using `prev` the initialization is placed on the TS reference.

  * `init` returns a reference to a new (initialized) dataflow variable, but maintains the *mutability* trait dataflow variable. 

  * Partial bits initialization is possible for `DFBits`. E.g. :

    `a.init(5,bits=(3,0))` Initializes only 

  * Initialization can be applied more than once. E.g. :

    `a.init(0).init(5,bits=(3,0))` Initializes all bits to 0 and then initializes a value of `5` to bits `3::0`

* Bubble tokens (Φ) :

  * Produced when a `prev` is called on a non-initialized dataflow variable. E.g.,

    | Code                                   | Init        | Token Stream          |
    | -------------------------------------- | ----------- | --------------------- |
    | `in : DFUInt(32)`                      | `Φ`         | `2, 3, 1, 5, 9`       |
    | `in.prev`                              | `Φ`         | `Φ, 2, 3, 1, 5, 9`    |
    | `in.prev(2)`                           | `Φ`         | `Φ, Φ, 2, 3, 1, 5, 9` |
    | `in.prev.prev`                         | `Φ`         | `Φ, Φ, 2, 3, 1, 5, 9` |
    | `val in1 = in.init(1); in1`            | `1`         | `2, 3, 1, 5, 9`       |
    | `in1.prev`                             | `1`         | `1, 2, 3, 1, 5, 9`    |
    | `in1.prev(2)`                          | `1`         | `1, 1, 2, 3, 1, 5, 9` |
    | `in1.prev.init(8)`                     | `8`         | `1, 2, 3, 1, 5, 9`    |
    | `val innew = DFUInt(32) := in1; innew` | `Φ`         | `2, 3, 1, 5, 9`       |
    | `val ins7 = in.init(7, Φ); ins7`       | `(7, Φ)`    | `2, 3, 1, 5, 9`       |
    | `ins7.prev`                            | `Φ`         | `7, 2, 3, 1, 5, 9`    |
    | `val ins78 = in.init(7, 8, Φ); ins78`  | `(7, 8, Φ)` | `2, 3, 1, 5, 9`       |
    | `ins78.prev`                           | `(8, Φ)`    | `7, 2, 3, 1, 5, 9`    |
    | `ins78.prev(2)`                        | `Φ`         | `8, 7, 2, 3, 1, 5, 9` |
    | `in.init(7).prev.init(8, Φ).prev`      | `Φ`         | `8, 7, 2, 3, 1, 5, 9` |

    ​

  * Bubbles are like any regular-value tokens in terms of consumption and production rules.

  * Bubbles are used to set phases between synchronized token streams by acting as a token place-holder within the stream queue.

  * Unless stated otherwise, any operation with a bubble token produces a bubble token (consuming the non-bubble token.). E.g., 

    ```scala
    def foo(a : DFUInt(8)) = a + a.prev
    //'in' is token stream of:    2, 3, 1, 5, 9
    //'foo(in)' returns:          Φ, 5, 4, 6, 14
    ```

  * `prev` maintains *Distributivity* through basic operations e.g.: 

    `(a + b).prev` ≗ `a.prev + b.prev` (timeless TS equality).

    | Code                                  | Init                                    | Token Stream                             |
    | ------------------------------------- | --------------------------------------- | ---------------------------------------- |
    | `inL : DFUInt(32)`                    | `Φ`                                     | `2, 3, 1, 5, 9`                          |
    | `inR : DFUInt(32)`                    | `Φ`                                     | `4, 0, 2`                                |
    | `inL + inR`                           | `Φ` `+`<br />`Φ` `=`<br />`Φ`           | `2, 3, 1, 5, 9` `+`<br />`4, 0, 2` `=`<br />`6, 3, 3` |
    | `inL + inR.prev`                      | `Φ` `+`<br />`Φ` `=`<br />`Φ`           | `2, 3, 1, 5, 9` `+`<br />`Φ, 4, 0, 2` `=`<br />`Φ, 7, 1, 7` |
    | `inL.init(1) + inR.init(3).prev`      | `1` `+`<br />`3` `=`<br />`4`           | `2, 3, 1, 5, 9` `+`<br />`3, 4, 0, 2` `=`<br />`5, 7, 1, 7` |
    | `inL.init(1, Φ) + inR.init(3).prev`   | `(1, Φ)` `+`<br />`3` `=`<br />`(4, Φ)` | `2, 3, 1, 5, 9` `+`<br />`3, 4, 0, 2` `=`<br />`5, 7, 1, 7` |
    | `inL.init(1) + inR.init(3, Φ).prev`   | `1` `+`<br />`Φ` `=`<br />`Φ`           | `2, 3, 1, 5, 9` `+`<br />`3, 4, 0, 2` `=`<br />`5, 7, 1, 7` |
    | `inL.init(1).prev + inR.init(3).prev` | `1` `+`<br />`3` `=`<br />`4`           | `1, 2, 3, 1, 5, 9` `+`<br />`3, 4, 0, 2` `=`<br />`4, 6, 3, 3` |
    | `(inL.init(1) + inR.init(3)).prev`    | `1` `+`<br />`3` `=`<br />`4`           | `(2, 3, 1, 5, 9` `+`<br />`4, 0, 2)` `.prev =`<br />`4, 6, 3, 3` |

    ​

  * We probably need to add `isBubble` and `produceBubble` to create unique control logic for Bubble tokens. 

  * Bubbles are typically treated differently at the edges of the dataflow paths, when bridging to the real-time world. E.g.: not committing bubbles to memory cells, or not raising ready flags.

* Formally comparing streams/variables:

  * Comparing variable references with/out initialization.
  * Comparing token stream references.
  * Comparing all tokens with/out bubbles, with/out time.

* Casting:

  * Parts of a bits vector can be bubbles while others normal values.

  * However, when casting to a number (e.g., DFUInt), the casting must check validity of all bits.

    If some of the bits are bubble then the entire number is considered as a bubble. Should there be a compilation warning/error ? 

* Invoking reinitialization:

  * For dataflow variables we can request reinitialization`in.reInit(cond)`

  ```scala
  def fib(restartReq : DFBool) = {
    val out = DFUInt(32).init(1, 0)
    out := out.prev + out.prev(2)
    out.reInit(restartReq) //when true, at the next iteration, prev<-1 and prev(2)<-0
    out.prev(2)
  }
  ```

  * Reinitialization sets the internal state-machine of `init` back to its IDLE state.
  * Do we consider the reinitialization condition as an unstruck-repeater implicitly?
  * Reinitialization is a local reset-like behavior that effects only the `prev` values at the next iteration.
  * Reinitialization is a non-violent change. If there is no consumer for `out` then the reinitialization will not take place. It waits just like any value token effect.
  * For reinitialization/reset methodology we have to consider Token Generators' state-machines separately from input-dependent state-machines. The input-dependent SM 

