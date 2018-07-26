package DFiant

import DFiant.internals._

import scala.collection.mutable.ListBuffer

trait DFConstruct extends HasProperties with Nameable with TypeNameable with Discoverable {

}

trait DFOwnerConstruct extends DFConstruct {
  protected implicit val blk : this.type = this

  private var idCnt : Int = 0
  final protected[DFiant] def getNewID(run : => Unit) : Int = {run; idCnt += 1; idCnt}

  protected[DFiant] val keepList : ListBuffer[Discoverable] = ListBuffer.empty[Discoverable]
  final def keep : this.type = {
    keepList += this
    this
  }
  protected def discoveryDepenencies : List[Discoverable] = keepList.toList

}