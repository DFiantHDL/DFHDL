package dfhdl.internals
import scala.quoted.*

object ControlledMacroError:
  // if contains a key, it means to activate error control.
  // if the value is empty (by default), it means the implicit given is found.
  // if the value is not empty, it means the implicit given with the error message.
  private val positionError = collection.concurrent.TrieMap.empty[String, String]
  private def getKey(using Quotes): String =
    import quotes.reflect.*
    Position.ofMacroExpansion.toString
  def activate()(using Quotes): Unit = positionError += getKey -> ""
  def deactivate()(using Quotes): Unit = positionError.remove(getKey)
  def getLastError(using Quotes): String = positionError.getOrElse(getKey, "")
  def getLastErrorAndDeactivate(using Quotes): String = positionError.remove(getKey).getOrElse("")
  def report(msg: String)(using Quotes): Expr[Nothing] =
    import quotes.reflect.report as macroReport
    val key = getKey
    if (positionError.contains(key))
      positionError += key -> msg
      macroReport.errorAndAbort(msg)
    else
      '{ compiletime.error(${ Expr(msg) }) }
end ControlledMacroError
