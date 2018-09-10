package DFiant.internals

sealed abstract class AliasReference(aliasCodeString_ : => String) {
  lazy val aliasCodeString : String = aliasCodeString_
}
object AliasReference {
  class AsIs(aliasCodeString : => String) extends AliasReference(aliasCodeString)
  object AsIs {
    def apply(aliasCodeString : => String) = new AsIs(aliasCodeString)
    def unapply(arg: AsIs): Boolean = true
  }
  class BitsWL(val relWidth : Int, val relBitLow : Int, aliasCodeString : => String) extends AliasReference(aliasCodeString)
  object BitsWL {
    def apply(relWidth: Int, relBitLow : Int, aliasCodeString : => String) = new BitsWL(relWidth, relBitLow, aliasCodeString)
    def unapply(arg : BitsWL): Option[(Int, Int)] = Some((arg.relWidth, arg.relBitLow))
  }
  class Prev(val step : Int) extends AliasReference(if (step == 1) ".prev" else s".prev($step)")
  object Prev {
    def apply(step : Int) = new Prev(step)
    def unapply(arg: Prev): Option[Int] = Some(arg.step)
  }
  class BitReverse(aliasCodeString : => String) extends AliasReference(aliasCodeString)
  object BitReverse {
    def apply(aliasCodeString : => String) = new BitReverse(aliasCodeString)
    def unapply(arg: BitReverse): Boolean = true
  }
  class Invert(aliasCodeString : => String) extends AliasReference(aliasCodeString)
  object Invert {
    def apply(aliasCodeString : => String) = new Invert(aliasCodeString)
    def unapply(arg: Invert): Boolean = true
  }
}
