package DFiant.compiler.stages
import DFiant.compiler.ir.*
import DFiant.compiler.analysis.*
import DFiant.compiler.printing.*
private class PrintCodeString(db: DB) extends Stage(db):
  override def transform: DB =
    given Printer = DefaultPrinter
    println(designDB.codeString)
    designDB

extension (db: DB) def printCodeString: DB = new PrintCodeString(db).transform
