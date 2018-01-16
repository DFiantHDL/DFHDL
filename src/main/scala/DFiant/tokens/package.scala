package DFiant

package object tokens {
  type Φ = Bubble
  final val Φ = Bubble

  implicit class TokenBitsSeq(seq : Seq[TokenBits]) extends TokenSeq(seq) {
    def unary_~ : Seq[TokenBits] = applyOp(TokenBits.unary_~)
    def ## (that : Seq[TokenBits]) : Seq[TokenBits] = applyOp(that, TokenBits.##)
  }

  implicit class TokenBoolSeq(seq : Seq[TokenBool]) extends TokenSeq(seq) {
    def unary_! : Seq[TokenBool] = applyOp(TokenBool.unary_!)
    def || (that : Seq[TokenBool]) : Seq[TokenBool] = applyOp(that, TokenBool.||)
    def && (that : Seq[TokenBool]) : Seq[TokenBool] = applyOp(that, TokenBool.&&)
  }

  implicit class TokenUIntSeq(seq : Seq[TokenUInt]) extends TokenSeq(seq) {
    def + (that : Seq[TokenUInt]) : Seq[TokenUInt] = applyOp(that, TokenUInt.+)
    def - (that : Seq[TokenUInt]) : Seq[TokenUInt] = applyOp(that, TokenUInt.-)
    def * (that : Seq[TokenUInt]) : Seq[TokenUInt] = applyOp(that, TokenUInt.*)
    def / (that : Seq[TokenUInt]) : Seq[TokenUInt] = applyOp(that, TokenUInt./)
    def % (that : Seq[TokenUInt]) : Seq[TokenUInt] = applyOp(that, TokenUInt.%)
    def < (that : Seq[TokenUInt]) : Seq[TokenBool] = applyOp(that, TokenUInt.<)
    def > (that : Seq[TokenUInt]) : Seq[TokenBool] = applyOp(that, TokenUInt.>)
    def <= (that : Seq[TokenUInt]) : Seq[TokenBool] = applyOp(that, TokenUInt.<=)
    def >= (that : Seq[TokenUInt]) : Seq[TokenBool] = applyOp(that, TokenUInt.>=)
    def == (that : Seq[TokenUInt]) : Seq[TokenBool] = applyOp(that, TokenUInt.==)
    def != (that : Seq[TokenUInt]) : Seq[TokenBool] = applyOp(that, TokenUInt.!=)
  }

  implicit class TokenSeqInit[T <: Token](tokenSeq : Seq[T]) {
    def prevInit(step : Int) : Seq[T] = {
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
