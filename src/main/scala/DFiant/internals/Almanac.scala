package DFiant.internals

import DFiant.DFAny.Pattern

import scala.collection.mutable._
class Almanac(val name : String, val owner : Option[Almanac]) {
  currentAlmanac =>
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

  implicit class EntryList(list : List[AlmanacEntry]) {
    def codeString : String = if (list.isEmpty) "" else list.map(e => e.codeString).mkString("\n  ", "\n  ", "")
  }
  implicit class Connection(conn : (AlmanacEntryNamed, AlmanacEntryNamed)) {
    def relativeRef(entry : AlmanacEntryNamed) : String = {
      //TODO: fix for the general case
      if (currentAlmanac eq entry.almanac) entry.name
      else s"${entry.almanac.name}.${entry.name}"
    }
    def codeString : String = s"${relativeRef(conn._2)} <> ${relativeRef(conn._1)}"
  }
  implicit class ConnectionList(list : List[(AlmanacEntryNamed, AlmanacEntryNamed)]) {
    def codeString : String = if (list.isEmpty) "" else list.map(e => e.codeString).mkString("\n  ", "\n  ", "")
  }

  lazy val allEntries : List[AlmanacEntry] = entries.toList
  lazy val allNamedEntries : List[AlmanacEntryNamed] = allEntries.collect{case e : AlmanacEntryNamed => e}
  lazy val allPorts : List[AlmanacEntryPort] = allNamedEntries.collect{case e : AlmanacEntryPort => e}
  lazy val dfVals : List[AlmanacEntryNewDFVar] = allNamedEntries.collect{case e : AlmanacEntryNewDFVar => e}
  lazy val inPorts : List[AlmanacEntryPort] = allPorts.filter(p => p.dir.isIn)
  lazy val outPorts : List[AlmanacEntryPort] = allPorts.filter(p => p.dir.isOut)
  lazy val connections : List[(AlmanacEntryNamed, AlmanacEntryNamed)] = {
    val oCons = outPorts.flatMap(p => p.sourceEntry match {
      case Some(s) => Some(Tuple2(s, p))
      case _ => None
    })
    val iCons = allComponents.flatMap(c => c.inPorts).flatMap(p => p.sourceEntry match {
      case Some(s) => Some(Tuple2(s, p))
      case _ => None
    })
    (oCons ++ iCons).distinct
  }


  def printEntrees() : Unit = {
    allEntries.map(e => {
//      if (e.codeString.startsWith("val "))
        println(e.codeString)
    })
  }

  def printConnections() : Unit = {
  }

  def printPorts() : Unit =  {
    allPorts.foreach(p => println(p.codeString))
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Components
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  def addComponent[A <: Almanac](almanac: A) : A = {
    require(phase == AlmanacPhaseConstruct, "Unexpected almanac component addition during a non-construction phase")
    components += almanac
    almanac
  }

  def fetchComponent[A <: Almanac](componentConstructor: => A) : A = {
    if (isSimulating)
      simulationComponentsIter.next().asInstanceOf[A]
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
  def bodyCodeString : String = s"${allPorts.codeString}${connections.codeString}${dfVals.codeString}"
  def codeString : String = s"val $name = new DFDesign {$bodyCodeString\n}"

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


class AlmanacIf(name : String, owner : Almanac, cond_ : => AlmanacEntryNamed)
  extends Almanac(name, Some(owner)) {
  final lazy val cond = cond_
  override def codeString : String = s"???"
}

class AlmanacElseIf(name : String, owner : Almanac, val prevIf : AlmanacIf, cond_ : => AlmanacEntryNamed)
  extends AlmanacIf(name, owner, cond_) {
  override def codeString : String = s"???"
}

class AlmanacElse(name : String, owner : Almanac, val previf : AlmanacIf)
  extends Almanac(name, Some(owner)) {
  override def codeString : String = s"???"
}

class AlmanacCasePattern(name : String, owner : Almanac, val prevCase : Option[AlmanacCasePattern], matchVal_ : => AlmanacEntryNamed, val pattern : Pattern[_])
  extends Almanac(name, Some(owner)) {
  final lazy val matchVal = matchVal_
  override def codeString : String = s"???"
}

class AlmanacCase_(name : String, owner : Almanac, val prevCase : Option[AlmanacCasePattern], matchVal_ : => AlmanacEntryNamed)
  extends Almanac(name, Some(owner)) {
  final lazy val matchVal = matchVal_
  override def codeString : String = s"???"
}
