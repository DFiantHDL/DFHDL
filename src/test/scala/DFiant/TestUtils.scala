package DFiant

import org.scalacheck.Prop

object TestUtils {
  def sameType[A, B](implicit ev: A =:= B): Boolean = true

  def wellTyped(body: => Unit): Prop = Prop.secure {
    body
    true
  }

  //nf = unsafe. used to force a not-final value. e.g., nf(3) returns a non-literal 3
  def us[T](t : T) : T = {
    var ret = t
    ret
  }

  def illRun(body: => Unit) : Boolean = {
    val isIll = try {
      body
      false
    } catch {
      case _ : Throwable =>
        true
    }
    if (!isIll)
      assert(false, "Expected assertion did not occur")
    true
  }
}
