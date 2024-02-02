package dfhdl.core
import dfhdl.compiler.ir
import dfhdl.internals.*

import scala.annotation.targetName

sealed abstract class DFError(
    val dfMsg: String
) extends Exception(dfMsg)
    derives CanEqual

object DFError:
  final class Basic(
      val opName: String,
      val iae: IllegalArgumentException
  )(using dfc: DFC)
      extends DFError(iae.getMessage):
    import dfc.getSet
    val designName = dfc.ownerOption match
      case Some(owner) => owner.asIR.getThisOrOwnerDesign.getFullName
      case None        => ""
    val fullName =
      if (dfc.isAnonymous) designName
      else if (designName.nonEmpty) s"$designName.${dfc.name}"
      else dfc.name
    val position = dfc.position
    override def toString: String =
      s"""|DFiant HDL elaboration error!
          |Position:  ${position}
          |Hierarchy: ${fullName}
          |Operation: `${opName}`
          |Message:   ${dfMsg}""".stripMargin
  end Basic
  object FakeEnum extends DFError("This value of enum is no meant to be accessed.")
  final class Derived(from: DFError) extends DFError(from.dfMsg)

  extension (dfErr: DFError)
    inline def asNet: DFNet = new DFNet(dfErr)
    inline def asFE[T <: DFTypeAny]: T = DFType(dfErr).asInstanceOf[T]
    inline def asValOf[T <: DFTypeAny]: DFValOf[T] = DFVal[T, ModifierAny, DFError](dfErr)
    inline def asVal[T <: DFTypeAny, M <: ModifierAny]: DFVal[T, M] = DFVal[T, M, DFError](dfErr)
end DFError

class Logger:
  private[Logger] var errors: List[DFError] = Nil
  def logError(err: DFError): Unit =
    errors = err :: errors
  def injectErrors(fromLogger: Logger): Unit =
    errors = fromLogger.errors ++ errors
  def getErrors: List[DFError] = errors.reverse
  def clearErrors(): Unit = errors = Nil

//@targetName("tryDFType")
//def trydf[T <: DFTypeAny](block: => T): T =
//  try block
//  catch
//    case e: IllegalArgumentException => DFError.Basic(e).asFE[T]
//    case e: DFError                  => e.asFE[T]
//
//@targetName("tryDFToken")
//def trydf[T <: DFTypeAny](block: => DFToken[T]): DFToken[T] =
//  try block
//  catch
//    case e: IllegalArgumentException => DFError.Basic(e).asTokenOf[T]
//    case e: DFError                  => e.asTokenOf[T]

@targetName("tryDFVal")
@metaContextForward(0)
def trydf[V <: DFValAny](
    block: => V
)(using dfc: DFC, ctName: CTName): V =
  try
    val ret = block
    import dfc.getSet
    val retIR = ret.asIR
    retIR.asVal[DFTypeAny, ModifierAny].asInstanceOf[V]
  catch
    case e: Exception =>
      val dfErr = e match
        case e: IllegalArgumentException => DFError.Basic(ctName.value, e)
        case e: DFError                  => e
        case e                           => throw e
      if (dfc.ownerOption.isEmpty)
        exitWithError(dfErr.toString())
      dfc.logError(dfErr)
      dfErr.asVal[DFTypeAny, ModifierAny].asInstanceOf[V]

@targetName("tryDFNet")
@metaContextForward(0)
def trydf(block: => Unit)(using dfc: DFC, ctName: CTName): Unit =
  try block
  catch
    case e: Exception =>
      val dfErr = e match
        case e: IllegalArgumentException => DFError.Basic(ctName.value, e)
        case e: DFError                  => e
        case e                           => throw e
      dfc.logError(dfErr)

def exitWithError(msg: String): Unit =
  System.err.println(msg)
  sys.exit(1)
