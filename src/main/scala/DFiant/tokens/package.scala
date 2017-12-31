package DFiant

package object tokens {
  type Φ = Bubble
  final val Φ = Bubble

  abstract class TokenSeq[T <: Token](seq : Seq[T]) {
    def applyOp(that : Seq[T], op : (T, T) => T) : Seq[T] = {
      val (smaller, larger) = if (seq.length < that.length) (seq, that.seq) else (that.seq, seq)
      val filler = Seq.fill(larger.length-smaller.length)(smaller.head)
      (filler ++ smaller).zip(larger).map(t => op(t._1, t._2))
    }
  }

  implicit class TokenBitsSeq(seq : Seq[TokenBits]) extends TokenSeq(seq) {
    def + (that : Seq[TokenBits]) : Seq[TokenBits] = applyOp(that, TokenBits.+)
    def ## (that : Seq[TokenBits]) : Seq[TokenBits] = ???
  }

  implicit class TokenSeqInit(tokenSeq : Seq[Token]) {
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
