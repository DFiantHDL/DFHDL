package sourcecode

object TermChecks {
  def run() = {
    def isVarCheck(implicit check : IsVar) : Unit = {}
    var varCheck = isVarCheck

    def isDefCheck(implicit check : IsDef) : Unit = {}
    def defCheck = isDefCheck
  }
}
