package DFiant.internals

import DFiant.tokens._

sealed class AlmanacInit(tokenSeq : Seq[Token]) {
  lazy val currentToken : Token = if (tokenSeq.isEmpty) BubbleToken else tokenSeq.head
  def prevInit(step : Int) : AlmanacInit = {
    val length = tokenSeq.length
    //No init at all, so invoking prev does not change anything (bubble tokens will be used)
    if ((length == 0) || (step == 0)) this
    //The step is larger or equals to the init sequence, so only the last init token remains
    else if (length <= step) AlmanacInit(tokenSeq.last)
    //More tokens are available than the step size, so we drop the first, according to the step count
    else AlmanacInit(tokenSeq.drop(step))
  }
  def +(rhs : AlmanacInit) : AlmanacInit = ???
  def apply(op : Seq[Token] => Token, args : AlmanacInit*) : AlmanacInit = ???
}

object AlmanacInit {
  case object Bubble extends AlmanacInit(Seq())

  def apply(token : Token) : AlmanacInit = new AlmanacInit(Seq(token))
  def apply(tokenSeq : Seq[Token]) : AlmanacInit = new AlmanacInit(tokenSeq)
}
