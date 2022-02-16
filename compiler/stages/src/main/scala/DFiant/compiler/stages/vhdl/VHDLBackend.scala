package DFiant.compiler.stages.vhdl

import DFiant.compiler.stages.*
import DFiant.compiler.analysis.*
import DFiant.compiler.ir.*
import DFiant.compiler.printing.*

private class VHDLBackend(db: DB) extends Stage(db):
  override protected def preTransform: DB =
    val updatedDB = db.dropUnreferenced.uniqueDesigns
    val designNames = updatedDB.members.collect { case block: DFDesignBlock => block.dclName }
    updatedDB.uniqueNames(designNames.toSet, caseSensitive = true) // .toLLRT
  override def transform: DB =
    println(toVHDL)
    designDB
  def toVHDL: String =
    given Printer = new RTPrinter
    designDB.codeString

extension [T: HasDB](t: T)
  def printVHDLCode: DB = new VHDLBackend(t.db).transform
  def getVHDLCode: String = new VHDLBackend(t.db).toVHDL
