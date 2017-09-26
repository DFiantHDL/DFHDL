package DFiant.internals

import scala.collection.immutable.Vector

trait AlmanacCondTree {
  self =>
  import AlmanacCondTree._
  /*
  GCT = 0|0

  ifdf () {
    GCT = 1|0.0|0
    ...
  } GCT = 1|0
  elseifdf () {
    GCT = 1|1.0|0
    ...
    ifdf () {
      GCT = 1|1.1|0.0|0
      ...
    } GCT = 1|1.1|0
  } GCT = 1|1
  elsedf {
    GCT = 1|2.0|0
    ...
  } GCT = 1|2

  if () {
    GCT = 2|0.0|0
  } GCT = 2|0
  */

  protected val condVec : Vector[Cond]

  //called when entering an ifdf
  def enterIf : AlmanacCondTree = new AlmanacCondTree {
    val condVec : Vector[Cond] = self.condVec.init :+ self.condVec.last.nextIf :+ Cond(0,0)
  }

  //called when exiting an ifdf
  def exitIf : AlmanacCondTree = new AlmanacCondTree {
    val condVec : Vector[Cond] = self.condVec.dropRight(1)
  }

  //called when entering an elsedf or elseifdf
  def enterElse : AlmanacCondTree = new AlmanacCondTree {
    val condVec : Vector[Cond] = self.condVec.init :+ self.condVec.last.nextElse :+ Cond(0,0)
  }

  //called when exiting an elsedf or elseifdf
  def exitElse : AlmanacCondTree = exitIf

  override def toString: String = condVec mkString "."
}

object AlmanacCondTree {
  case class Cond(ifIdx : Int, elseIdx : Int) {
    def nextIf : Cond = Cond(ifIdx + 1, 0)
    def nextElse : Cond = Cond(ifIdx, elseIdx + 1)

    override def toString: String = s"$ifIdx|$elseIdx"
  }

  def apply() : AlmanacCondTree = new AlmanacCondTree {protected val condVec : Vector[Cond] = Vector(Cond(0,0))}
}

sealed trait AlmanacTimeRef
case class AlmanacTimeRefCurrent() extends AlmanacTimeRef {
  override def toString: String = "C"
}
case class AlmanacTimeRefPast(idx : Int) extends AlmanacTimeRef {
  override def toString: String = "P" + idx
}
case class AlmanacTimeRefFuture(idx : Int) extends AlmanacTimeRef {
  override def toString: String = "F" + idx
}

sealed trait AlmanacAddress
object AlmanacAddressLatest extends AlmanacAddress
trait AlmanacAddressSpecific extends AlmanacAddress {
  self =>

  //Time reference of the address
  //Negative value - Past
  //Zero (0) value - Current/Present time
  //Positive value - Future
  val timeRef : AlmanacTimeRef

  //Conditional Tree
  val condTree : AlmanacCondTree

  //Assignment Index
  val assignIdx : Int

  def newAssignment = new AlmanacAddressSpecific {
    val timeRef: AlmanacTimeRef = self.timeRef
    val assignIdx: Int = self.assignIdx+1
    val condTree: AlmanacCondTree = self.condTree
  }

  override def toString: String = s"${timeRef}_${condTree}_$assignIdx"
}


object AlmanacAddress {
  def init() : AlmanacAddressSpecific = new AlmanacAddressSpecific {
    override val timeRef: AlmanacTimeRef = AlmanacTimeRefCurrent()
    override val assignIdx: Int = 0
    override val condTree: AlmanacCondTree = AlmanacCondTree()
  }
}