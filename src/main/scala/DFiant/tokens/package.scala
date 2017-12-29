package DFiant

package object tokens {
  type Φ = Bubble
  final val Φ = Bubble

  implicit class TokenBitsSeq(seq : Seq[TokenBits]) {
    def + (that : Seq[TokenBits]) : Seq[TokenBits] = ???
    def ## (that : Seq[TokenBits]) : Seq[TokenBits] = ???
  }

}
