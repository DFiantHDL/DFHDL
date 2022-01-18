package DFiant.compiler.stages
import DFiant.compiler.ir.*
import DFiant.compiler.analysis.*
import DFiant.compiler.printing.*
private class PrintCodeString(db: DB) extends Stage(db):
  override protected def preTransform: DB =
    db.dropUnreferenced
      .uniqueNames(Set(), caseSensitive = true)
  override def transform: DB =
    println(codeString)
    designDB
  def codeString: String =
    given Printer = DefaultPrinter
    designDB.codeString

extension [T: HasDB](t: T)
  def printCodeString: DB = new PrintCodeString(t.db).transform
  def getCodeString: String = new PrintCodeString(t.db).codeString
