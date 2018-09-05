package DFiant.internals

sealed trait AliasReference {
  val aliasCodeString : String
}
object AliasReference {
  case class AsIs(aliasCodeString : String) extends AliasReference
  case class BitsWL(relWidth : Int, relBitLow : Int, aliasCodeString : String) extends AliasReference
  case class Prev(step : Int) extends AliasReference {
    val aliasCodeString : String = if (step == 1) ".prev" else s".prev($step)"
  }
  case class BitReverse(aliasCodeString : String) extends AliasReference
  case class Invert(aliasCodeString : String) extends AliasReference
}
