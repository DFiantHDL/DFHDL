package dfhdl.sim

/** DFacsimile kernel mockup — execution contract shared by all tiers.
  *
  * The state/signal array is the mockup stand-in for the planned `StateLayout`: registers and
  * combinational values live in one flat `Array[Long]` so that the interpreter and codegen tiers
  * can be hot-swapped over the same state, and checkpoints are a plain copy.
  */
trait SimKernel:
  /** Advance the simulation by `cycles` clock cycles over the given signal/state array. */
  def run(sig: Array[Long], cycles: Long): Unit

  /** Evaluate the combinational sweep only, without committing registers — the settle-on-peek
    * primitive: observers always read combinationally settled state.
    */
  def settle(sig: Array[Long]): Unit
