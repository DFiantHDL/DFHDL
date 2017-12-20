package DFiant.internals

trait AlmanacID

case class AlmanacIDConst(constVal : BigInt) extends AlmanacID {
  override def toString: String = s"CONST_$constVal"
}


trait AlmanacIDUnique extends AlmanacID {
  protected val unique : Int

  override def toString: String = s"V$unique"
}

object AlmanacID {
  protected var idsNum : Int = 0

  def apply() : AlmanacID = {
    val ret = new AlmanacIDUnique {protected val unique : Int = idsNum}
    idsNum += 1
    ret
  }
}


