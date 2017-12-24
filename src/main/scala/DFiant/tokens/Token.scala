package DFiant.tokens

class Token {

}
object Token {
  def `+`(arg0 : Token, arg1 : Token) : Token = ???
  def apply(value : BigInt) : Token = ???
}
case object BubbleToken extends Token //TODO
case object ZeroToken extends Token //TODO