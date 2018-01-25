package DFiant.internals

import scala.collection.mutable._
object Almanac {
  val printEntreesFlag : Boolean = true
  private var currentAddress : AlmanacAddressSpecific = AlmanacAddress.init()
  private var phase : AlmanacPhase = AlmanacPhaseConstruct
  private val list : ListBuffer[AlmanacEntry] = ListBuffer.empty[AlmanacEntry]
  private var simulationIter = list.iterator

  def isSimulating : Boolean = phase == AlmanacPhaseSimulate
  def clear() : Unit = {
    currentAddress = AlmanacAddress.init()
    phase = AlmanacPhaseConstruct
    list.clear()
  }

  def getCurrentAddress : AlmanacAddressSpecific = currentAddress
  def addEntry(almanacEntry: AlmanacEntry) : Unit = {
    require(phase == AlmanacPhaseConstruct, "Unexpected almanac entry addition during a non-construction phase")
    list += almanacEntry
  }

  def fetchEntry[AE <: AlmanacEntry](entryConstructor: => AE) : AE = {
    if (isSimulating)
      simulationIter.next().asInstanceOf[AE]
    else
      entryConstructor
  }

  def getList = list
  def newSimPhase() : Unit = {
    phase = AlmanacPhaseSimulate
    simulationIter = list.iterator
  }

  def printEntrees() : Unit = {
    list.map(e => println(e.codeString))
  }
}


