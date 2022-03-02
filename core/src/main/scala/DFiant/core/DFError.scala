package DFiant.core
import DFiant.compiler.ir
import DFiant.internals.Position

sealed trait DFError extends Product, Serializable derives CanEqual
object DFError:
  sealed trait Details extends DFError:
    val msg: String
    val designOwner: Option[ir.DFDesignBlock]
    val pos: Position
  case object Derived extends DFError

////////////////////////////////////////////////////////////////////////////////////
// Exception handling for DFiant code errors
////////////////////////////////////////////////////////////////////////////////////
def errordf(msg: String)(using dfc: DFC): Nothing =
  import scala.io.AnsiColor.{RED, RESET}
  import dfc.getSet
  val designName = dfc.owner.asIRForced.getThisOrOwnerDesign.getFullName
  val fullName =
    if (dfc.isAnonymous) designName
    else s"$designName.${dfc.name}"
  println(
    s"${RED}DFiant HDL compilation failed at:\n${dfc.position}\nin:\n$fullName\nwith:\n$msg$RESET"
  )
  sys.exit(1)

def trydf[T](block: => T)(using DFC): T =
  try block
  catch
    case e: IllegalArgumentException =>
      errordf(e.getMessage.replaceFirst("requirement failed: ", ""))
////////////////////////////////////////////////////////////////////////////////////
