package dfhdl.sim

/** DFacsimile kernel mockup — execution contract shared by all tiers.
  *
  * The state/signal array is the mockup stand-in for the planned `StateLayout`: registers and
  * combinational values live in one flat `Array[Long]` so that the interpreter and codegen tiers
  * can be hot-swapped over the same state, and checkpoints are a plain copy.
  */
trait SimKernel:
  /** Install the reset image of each memory (indexed by memory id) into the kernel's backing store.
    * Called once after construction; memory-less kernels ignore it. The kernel owns the working copy
    * that writes mutate; `mems` is the reset image and is not retained.
    */
  def initMem(mems: Array[Array[Long]]): Unit = ()

  /** Advance the simulation by `cycles` clock cycles over the given signal/state array. */
  def run(sig: Array[Long], cycles: Long): Unit

  /** Advance up to `cycles` clock cycles, stopping after any cycle whose settled `watch` value is
    * nonzero (that cycle still commits — the caller observes the fired cycle's swept values, which
    * survive the commit in the signal array). Returns the number of cycles consumed. This is the
    * text-output hook: the watch node aggregates all action guards, so a bulk run costs a single
    * signal read per cycle when nothing fires.
    */
  def runWatch(sig: Array[Long], cycles: Long, watch: Int): Long

  /** Evaluate the combinational sweep only, without committing registers — the settle-on-peek
    * primitive: observers always read combinationally settled state.
    */
  def settle(sig: Array[Long]): Unit

  /** Advance exactly one cycle with change tracking: returns true when any *tracked* register
    * commit changed its value. A clean (false) cycle proves the design is at a state fixpoint
    * except for untracked time-keeping cells (wait counters), which is the scheduler's license to
    * skip ahead on the event timeline instead of evaluating every cycle. The bulk [[run]] path
    * carries no tracking overhead.
    */
  def stepDirty(sig: Array[Long]): Boolean
end SimKernel
