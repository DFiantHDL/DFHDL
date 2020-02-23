package ZFiant.compiler.backend

import collection.mutable

package object utils {
  implicit class Delimiter(s : String) {
    import DFiant.internals.StringExtras
    def delim : String = s.delimRowsBy("  ")
  }

  final class NameDB[N](reservedNames : List[String], caseSensitive : Boolean, gen : String => N) {
    //initializing the table with reserved names
    //starting from -1 for reserved names, so the first indexed returned name value will be 0.
    //this is different than non-reserved name collision which will start at index 1
    private val nameTable : mutable.HashMap[String, Int] = mutable.HashMap.from(reservedNames.map(r => (r, -1)))
    private def getUniqueName(suggestedName : String) : String = {
      val lcSuggestedName = if (caseSensitive) suggestedName else suggestedName.toLowerCase()
      nameTable.get(lcSuggestedName) match {
        case Some(v) =>
          nameTable.update(lcSuggestedName, v + 1)
          suggestedName + "_b_" + v //_b_ for Backend indication
        case _ =>
          nameTable.update(lcSuggestedName, 1)
          suggestedName
      }
    }
    def apply(value : String) : N = gen(getUniqueName(value))
  }
}
