package DFiant.core
import DFiant.compiler.ir
import DFiant.compiler.ir.DFVal.ModifierAny
import DFiant.internals.Position

import scala.annotation.targetName

sealed abstract class DFError(
    val dfMsg: String
) extends Exception(dfMsg)
    derives CanEqual

object DFError:
  final class Basic(
      iae: IllegalArgumentException
  ) extends DFError(iae.getMessage)
  final class Details(
      dfMsg: String,
      val dfDesign: Option[ir.DFDesignBlock],
      val pos: Position
  ) extends DFError(dfMsg)
  final class Derived(from: DFError) extends DFError(from.dfMsg)

  extension (dfErr: DFError)
    def asFE[T <: DFTypeAny]: T = DFType(dfErr).asInstanceOf[T]
    def asValOf[T <: DFTypeAny]: DFValOf[T] = new DFVal[T, ModifierAny](dfErr)
    def asVal[T <: DFTypeAny, M <: ModifierAny]: DFVal[T, M] = new DFVal[T, M](dfErr)
    def asTokenOf[T <: DFTypeAny]: DFToken[T] = new DFToken[T](dfErr)
end DFError

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

@targetName("tryDFType")
def trydf[T <: DFTypeAny](block: => T): T =
  try block
  catch
    case e: IllegalArgumentException => DFError.Basic(e).asFE[T]
    case e: DFError                  => e.asFE[T]

@targetName("tryDFToken")
def trydf[T <: DFTypeAny](block: => DFToken[T]): DFToken[T] =
  try block
  catch
    case e: IllegalArgumentException => DFError.Basic(e).asTokenOf[T]
    case e: DFError                  => e.asTokenOf[T]

@targetName("tryDFVal")
def trydf[T <: DFTypeAny, M <: ModifierAny](block: => DFVal[T, M])(using DFC): DFVal[T, M] =
  try block
  catch
    case e: Exception =>
      val dfErr = e match
        case e: IllegalArgumentException => DFError.Basic(e)
        case e: DFError                  => e
      dfc.logError(dfErr)
      dfErr.asVal[T, M]

//def trydf[T](block: => T)(using DFC): T =
//  try block
//  catch
//    case e: IllegalArgumentException =>
//      errordf(e.getMessage.replaceFirst("requirement failed: ", ""))
////////////////////////////////////////////////////////////////////////////////////
