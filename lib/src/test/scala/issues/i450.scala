package issues.i450

import dfhdl.*

// A `Bits` port whose width comes from a design parameter that is USED THROUGH ITS DEFAULT
// (`new Consumer()`), tied to `all(0)` by the parent. A class parameter default is evaluated
// inside the child's own context, so the default's literal is a child-context member; the
// `all(0)` width resolution reaches it through the applied/default path after the child has
// ended, and a simplification returns it in the parent's context. Applying the context meta
// to that result by MUTATION used to crash with an internal `NoSuchElementException` (a
// `memberTable` miss: the member belongs to the ended child context). Passing the same value
// explicitly (`new Consumer(W = 36)`) never crashed, since the applied literal is created at
// the call site, in the parent's context.
class Consumer(val W: Int <> CONST = 36) extends EDDesign:
  val i = Bits(W) <> IN
  val o = Bits(W) <> OUT
  o <> i

@top(false) class TopC extends EDDesign:
  val q = Bit <> OUT
  q <> 0
  val c = new Consumer()
  c.i <> all(0)
  c.o <> OPEN
