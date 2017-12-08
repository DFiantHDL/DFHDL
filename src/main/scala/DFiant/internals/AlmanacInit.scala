package DFiant.internals

trait AlmanacInit {
  def currentInit : AlmanacInitValue
  def prevInit : AlmanacInit
}

case class AlmanacInitValue() extends AlmanacInit {

  val bitsEnableBigInt = BigInt(0)
  val bitsValueBigInt = BigInt(0)
  def currentInit : AlmanacInitValue = this
  def prevInit : AlmanacInit = this
}

//case class AlmanacInitSeq() extends AlmanacInit {
//  val initSeq = Seq[AlmanacInitValue]()
//  def currentInit : AlmanacInitValue = initSeq.head
//}