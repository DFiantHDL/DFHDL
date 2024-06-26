[](){#state}
# State & Initialization

Semantically, every DFiant dataflow variable references a token stream (TS). 



* Unless stated otherwise, all dataflow variables are always consuming and always producing.

* Previous token initialization:

  * The token history stream can be initialized.

  * Initialization does not mutate the dataflow variable.

  * Initialization has no effect on the TS. Only when using `prev` the initialization is placed on the TS reference.

  * `init` returns a reference to a new (initialized) dataflow variable, but maintains the *mutability* trait dataflow variable. 

* Bubble tokens (?) :

  * Produced when a `prev` is called on a non-initialized dataflow variable. E.g.,
  
    | Code                                   | Init        | Token Stream          |
    | -------------------------------------- | ----------- | --------------------- |
    | `in : DFUInt[32]`                      | `?`         | `2, 3, 1, 5, 9`       |
    | `in.prev`                              | `?`         | `?, 2, 3, 1, 5, 9`    |
    | `in.prev(2)`                           | `?`         | `?, ?, 2, 3, 1, 5, 9` |
    | `in.prev.prev`                         | `?`         | `?, ?, 2, 3, 1, 5, 9` |
    | `val in1 = in.init(1); in1`            | `1`         | `2, 3, 1, 5, 9`       |
    | `in1.prev`                             | `1`         | `1, 2, 3, 1, 5, 9`    |
    | `in1.prev(2)`                          | `1`         | `1, 1, 2, 3, 1, 5, 9` |
    | `in1.prev.init(8)`                     | `8`         | `1, 2, 3, 1, 5, 9`    |
    | `val innew = DFUInt(32) := in1; innew` | `?`         | `2, 3, 1, 5, 9`       |
    | `val ins7 = in.init(7, ?); ins7`       | `(7, ?)`    | `2, 3, 1, 5, 9`       |
    | `ins7.prev`                            | `?`         | `7, 2, 3, 1, 5, 9`    |
    | `val ins78 = in.init(7, 8, ?); ins78`  | `(7, 8, ?)` | `2, 3, 1, 5, 9`       |
    | `ins78.prev`                           | `(8, ?)`    | `7, 2, 3, 1, 5, 9`    |
    | `ins78.prev(2)`                        | `?`         | `8, 7, 2, 3, 1, 5, 9` |
    | `in.init(7).prev.init(8, ?).prev`      | `?`         | `8, 7, 2, 3, 1, 5, 9` |
  
    
  
  * Bubbles are like any regular-value tokens in terms of consumption and production rules.
  
  * Bubbles are used to set phases between synchronized token streams by acting as a token place-holder within the stream queue.
  
  * Unless stated otherwise, any operation with a bubble token produces a bubble token (consuming the non-bubble token.). E.g., 
  
    ```scala
    def foo(a : DFUInt(8)) = a + a.prev
    //'in' is token stream of:    2, 3, 1, 5, 9
    //'foo(in)' returns:          ?, 5, 4, 6, 14
    ```
  
  * `prev` maintains *Distributivity* through basic operations e.g.: 
  
    `(a + b).prev` ≗ `a.prev + b.prev` (timeless TS equality).
  
    | Code                                  | Init                                    | Token Stream                             |
    | ------------------------------------- | --------------------------------------- | ---------------------------------------- |
    | `inL : DFUInt(32)`                    | `?`                                     | `2, 3, 1, 5, 9`                          |
    | `inR : DFUInt(32)`                    | `?`                                     | `4, 0, 2`                                |
    | `inL + inR`                           | `?` `+`<br />`?` `=`<br />`?`           | `2, 3, 1, 5, 9` `+`<br />`4, 0, 2` `=`<br />`6, 3, 3` |
    | `inL + inR.prev`                      | `?` `+`<br />`?` `=`<br />`?`           | `2, 3, 1, 5, 9` `+`<br />`?, 4, 0, 2` `=`<br />`?, 7, 1, 7` |
    | `inL.init(1) + inR.init(3).prev`      | `1` `+`<br />`3` `=`<br />`4`           | `2, 3, 1, 5, 9` `+`<br />`3, 4, 0, 2` `=`<br />`5, 7, 1, 7` |
    | `inL.init(1, ?) + inR.init(3).prev`   | `(1, ?)` `+`<br />`3` `=`<br />`(4, ?)` | `2, 3, 1, 5, 9` `+`<br />`3, 4, 0, 2` `=`<br />`5, 7, 1, 7` |
    | `inL.init(1) + inR.init(3, ?).prev`   | `1` `+`<br />`?` `=`<br />`?`           | `2, 3, 1, 5, 9` `+`<br />`3, 4, 0, 2` `=`<br />`5, 7, 1, 7` |
    | `inL.init(1).prev + inR.init(3).prev` | `1` `+`<br />`3` `=`<br />`4`           | `1, 2, 3, 1, 5, 9` `+`<br />`3, 4, 0, 2` `=`<br />`4, 6, 3, 3` |
    | `(inL.init(1) + inR.init(3)).prev`    | `1` `+`<br />`3` `=`<br />`4`           | `(2, 3, 1, 5, 9` `+`<br />`4, 0, 2)` `.prev =`<br />`4, 6, 3, 3` |
  
    
  
  * Bubbles are typically treated differently at the edges of the dataflow paths, when bridging to the real-time world. E.g.: not committing bubbles to memory cells, or not raising ready flags.
  
* Casting:

  * Parts of a bits vector can be bubbles while others normal values.

  * However, when casting to a number (e.g., DFUInt), the casting must check validity of all bits.



## Time Invariance