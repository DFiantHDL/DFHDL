package DFiant.compiler.stages
import DFiant.compiler.ir.*
import DFiant.compiler.analysis.*
import DFiant.compiler.printing.*
private class PrintCodeString(db: DB) extends Stage(db):
  override protected def preTransform: DB =
    db.dropUnreferenced
      .uniqueNames(Set(), caseSensitive = true)
  override def transform: DB =
    given Printer = DefaultPrinter
    println(designDB.codeString)
    designDB

extension [T: HasDB](t: T) def printCodeString: DB = new PrintCodeString(t.db).transform
