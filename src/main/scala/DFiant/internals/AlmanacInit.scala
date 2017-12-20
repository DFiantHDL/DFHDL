package DFiant.internals

trait AlmanacInit {
  def currentInit : AlmanacInitValue
  def prevInit : AlmanacInit
}

class AlmanacInitValue(val bitsEnableBigInt : BigInt, val bitsValueBigInt : BigInt) extends AlmanacInit {
  def currentInit : AlmanacInitValue = this
  def prevInit : AlmanacInit = this
}

case object AlmanacInitValueBubble extends AlmanacInitValue(0, 0)

//case class AlmanacInitSeq() extends AlmanacInit {
//  val initSeq = Seq[AlmanacInitValue]()
//  def currentInit : AlmanacInitValue = initSeq.head
//}