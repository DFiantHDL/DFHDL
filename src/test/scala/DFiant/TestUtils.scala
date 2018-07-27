package DFiant

import org.scalacheck.Prop
import singleton.twoface.TwoFace

object TestUtils {
  def sameType[A, B](implicit ev: A =:= B): Boolean = true

  def wellTyped(body: => Unit): Prop = Prop.secure {
    body
    true
  }

  implicit def tfToProp[B](tf : TwoFace.Boolean[B]) : Prop = tf.getValue

  //nf = not-final. used to force a not-final value. e.g., nf(3) returns a non-literal 3
  def nf(t : Int) = t
  def nf(t : Long) = t

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

  def trimWhites(s : String) : String = s.replaceAll("(?m)^[\\s&&[^\\n]]+|[\\s+&&[^\\n]]+$", "").trim.filter(_ >= ' ')

  implicit class StringEnhancer(s : String) {
    def =@= (that : String) : Boolean = trimWhites(s) == trimWhites(that)
  }
}
