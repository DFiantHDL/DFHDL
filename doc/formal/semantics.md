# Semantics

Semantically, each DFiant dataflow variable references a token stream (TS). 





Formalism Brainstorming:

* Unless stated otherwise, all dataflow variables are always consuming and always producing.

* Previous token initialization:

  * The token history stream can be initialized.

  * Initialization does not mutate the dataflow variable.

  * Initialization has no effect on the TS. Only when using `prev` the initialization is placed on the TS reference.

  * `init` returns a reference to a new (initialized) dataflow variable, but maintains the *mutability* trait dataflow variable. 

  * Partial bits initialization is possible. E.g. :

    `a.init(5,bits=(3,0))` Initializes only 

  * Initialization can be applied more than once. E.g. :

    `a.init(0).init(5,bits=(3,0))` Initializes all bits to 0 and then initializes a value of `5` to bits `3::0`

* Bubble tokens (Φ) :

  * Produced when a `prev` is called on a non-initialized dataflow variable. E.g.,

    ```scala
    //'in' is token stream of:    2, 3, 1, 5, 9
    //'in.prev' returns:          Φ, 2, 3, 1, 5, 9
    //'in.init(1).prev' returns:  1, 2, 3, 1, 5, 9
    ```

  * Bubbles are like any regular-value tokens in terms of consumption and production rules.

  * Unless stated otherwise, any operation with a bubble token produces a bubble token (consuming the non-bubble token.). E.g., 

    ```scala
    def foo(a : DFUInt(8)) = a + a.prev
    //'in' is token stream of:    2, 3, 1, 5, 9
    //'foo(in)' returns:          Φ, 5, 4, 6, 14
    ```

  * It is important that `prev` maintains *Distributivity* through basic operations, e.g.: 

    `(a + b).prev` ≗ `a.prev + b.prev` (timeless TS equality).

  * We probably need to add `isBubble` and `produceBubble` to create unique control logic for Bubble tokens. 

  * Bubbles are typically treated differently at the edges of the dataflow paths, when bridging to the real-time world. E.g.: not committing bubbles to memory cells, or not raising ready flags.

* Formally comparing streams/variables:

  * Comparing variable references with/out initialization.
  * Comparing token stream references.
  * Comparing all tokens with/out bubbles, with/out time.

* Null-sized vectors:

  * Bit selection/Aliasing in `Struct`, like 
  * Arithmetic result (bit shifting)



