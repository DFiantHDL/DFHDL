package DFiant.internals

import DFiant.core.DFAny.Token

trait AlmanacID

case class AlmanacIDConst(token : Token) extends AlmanacID {
  override def toString: String = s"$token"
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


