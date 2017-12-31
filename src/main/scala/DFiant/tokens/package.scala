package DFiant

package object tokens {
  type Φ = Bubble
  val Φ = Bubble

  implicit class TokenSeq(tokenSeq : Seq[Token]) {
    def + (that : Seq[Token]) : Seq[Token] = {
      ???
    }
    def prevInit(step : Int) : Seq[Token] = {
      val length = tokenSeq.length
      //No init at all, so invoking prev does not change anything (bubble tokens will be used)
      if ((length == 0) || (step == 0)) tokenSeq
      //The step is larger or equals to the init sequence, so only the last init token remains
      else if (length <= step) Seq(tokenSeq.last)
      //More tokens are available than the step size, so we drop the first, according to the step count
      else tokenSeq.drop(step)
    }
  }

}
