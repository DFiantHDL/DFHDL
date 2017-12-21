package DFiant.internals

import DFiant.tokens._

sealed trait AlmanacInit {
  val currentToken : Token
  val prevInit : AlmanacInit
}

object AlmanacInit {
  class FromToken(token : => Token) extends AlmanacInit {
    lazy val currentToken : Token = token
    lazy val prevInit : AlmanacInit = this
  }

  class FromTokenSeq(tokenSeq : => Seq[Token]) extends AlmanacInit {
    lazy val currentToken : Token = if (tokenSeq.isEmpty) BubbleToken else tokenSeq.head
    lazy val prevInit : AlmanacInit = AlmanacInit(tokenSeq.drop(1))
  }

  case object Bubble extends FromToken(BubbleToken)

  def apply(token : Token) : AlmanacInit = new FromToken(token)
  def apply(tokenSeq : Seq[Token]) : AlmanacInit = new FromTokenSeq(tokenSeq)
}
