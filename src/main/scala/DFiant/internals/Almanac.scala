package DFiant.internals

import scala.collection.mutable._
final class Almanac(val name : String, val owner : Option[Almanac]) {
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

  lazy val allEntries : List[AlmanacEntry] = entries.toList
  lazy val ports : List[AlmanacEntryPort] = allEntries.collect{case e : AlmanacEntryPort => e}
  lazy val connections : List[(AlmanacEntry, AlmanacEntry)] = {
    ports.flatMap(p => p.sourceEntry match {
      case Some(s) => Some(Tuple2(s, p))
      case _ => None
    }).distinct
  }


  def printEntrees() : Unit = {
    allEntries.map(e => {
//      if (e.codeString.startsWith("val "))
        println(e.codeString)
    })
  }

  def printConnections() : Unit = {
    connections.foreach(c => println(s"${c._2} <- ${c._1}"))
  }

  def printPorts() : Unit =  {
    ports.foreach(p => println(p.name))
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

  lazy val allComponents : List[Almanac] = components.toList

  def printComponents() : Unit = {
    allComponents.foreach(c => println(c.name))
  }

  def isTop : Boolean = owner.isEmpty
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Informational
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  lazy val fullName : String = owner match {
    case Some(o) => s"${o.fullName}.$name"
    case _ => name //Top
  }

  override def toString: String = name

  def printInfo() : Unit = {
    println(s"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
    println(s"Design $name")
    println(s"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
    println(
      """
        |Ports:
        |------""".stripMargin)
    printPorts()
    println(
      """
        |Components:
        |-----------""".stripMargin)
    printComponents()
    println(
      """
        |Connections:
        |------------""".stripMargin)
    printConnections
    println(
      """
        |Entrees:
        |--------
        |""".stripMargin)
    println(s"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////

}

