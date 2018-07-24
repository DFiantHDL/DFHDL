package DFiant.internals

import scala.collection.mutable._
class Almanac(val name : String, val owner : Option[Almanac]) {
  val printEntreesFlag : Boolean = true
  private var currentAddress : AlmanacAddressSpecific = AlmanacAddress.init()
  private var phase : AlmanacPhase = AlmanacPhaseConstruct
  private val components : ListBuffer[Almanac] = ListBuffer.empty[Almanac]
  private val entries : ListBuffer[AlmanacEntry] = ListBuffer.empty[AlmanacEntry]
  private var simulationEntriesIter = entries.iterator
  private var simulationComponentsIter = components.iterator

  def isSimulating : Boolean = phase == AlmanacPhaseSimulate
  def newSimPhase() : Unit = {
    components.foreach(c => c.newSimPhase())
    phase = AlmanacPhaseSimulate
    simulationEntriesIter = entries.iterator
    simulationComponentsIter = components.iterator
  }
  def clear() : Unit = {
    currentAddress = AlmanacAddress.init()
    phase = AlmanacPhaseConstruct
    entries.clear()
    components.clear()
  }
  def getCurrentAddress : AlmanacAddressSpecific = currentAddress

  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Entries
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  def addEntry(almanacEntry: AlmanacEntry) : Unit = {
    require(phase == AlmanacPhaseConstruct, "Unexpected almanac entry addition during a non-construction phase")
    entries += almanacEntry
  }

  def fetchEntry[AE <: AlmanacEntry](entryConstructor: => AE) : AE = {
    if (isSimulating)
      simulationEntriesIter.next().asInstanceOf[AE]
    else
      entryConstructor
  }

  def getEntries = entries.toList

  def printEntrees() : Unit = {
    entries.map(e => {
      if (e.codeString.startsWith("val "))
        println(e.codeString)
    })
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Components
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  def addComponent(almanac: Almanac) : Almanac = {
    require(phase == AlmanacPhaseConstruct, "Unexpected almanac component addition during a non-construction phase")
    components += almanac
    almanac
  }

  def fetchComponent(componentConstructor: => Almanac) : Almanac = {
    if (isSimulating)
      simulationComponentsIter.next()
    else
      componentConstructor
  }

  def getComponents = components.toList

  def printComponents() : Unit = {
    println(components)
  }

  final def isTop : Boolean = owner.isEmpty
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Informational
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  final lazy val fullName : String = owner match {
    case Some(o) => s"${o.fullName}.$name"
    case _ => name //Top
  }

  def printInfo() : Unit = {
    println(s"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
    println(s"Design $name")
    println(s"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
    println(
      """
        |Ports:
        |------""".stripMargin)
    println(
      """
        |Components:
        |-----------""".stripMargin)
    printComponents()
    println()
    println(
      """
        |Entrees:
        |--------""".stripMargin)
    println(s"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////

}

