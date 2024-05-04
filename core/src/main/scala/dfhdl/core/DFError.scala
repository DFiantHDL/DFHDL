package dfhdl.core
import dfhdl.compiler.ir
import dfhdl.internals.*

import scala.annotation.targetName

sealed abstract class DFError(
    val dfMsg: String
) extends Exception(dfMsg)
    derives CanEqual

object DFError:
  class Basic(
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
  final class REG_DIN[T <: DFTypeAny](val dfVar: DFVarOf[T])(using dfc: DFC)
      extends Basic(
        "Read access",
        new IllegalArgumentException(
          """|Cannot read from DIN of a register.
             |If you are committing a partial assignment through range or field selection, make sure you apply `.din` after the selection. E.g.:
             |* Instead of `x.din(5, 0)` write `x(5, 0).din`.
             |* Instead of `pixel.din.x` write `pixel.x.din`.
          """.stripMargin
        )
      ):
    var firstTime: Boolean = true

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

@targetName("tryDFType")
@metaContextForward(0)
def trydf[T <: DFTypeAny](
    block: => T
)(using dfc: DFC, ctName: CTName): T =
  if (dfc.inMetaProgramming) block
  else
    try block
    catch
      case e: Exception =>
        val dfErr = e match
          case e: IllegalArgumentException => DFError.Basic(ctName.value, e)
          case e: DFError                  => e
          case e                           => throw e
        if (dfc.ownerOption.isEmpty)
          exitWithError(dfErr.toString())
        dfc.logError(dfErr)
        new DFTypeAny(dfErr).asInstanceOf[T]

@targetName("tryDFVal")
@metaContextForward(0)
def trydf[V <: DFValAny](
    block: => V
)(using dfc: DFC, ctName: CTName): V =
  if (dfc.inMetaProgramming) block
  else
    try block
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
  if (dfc.inMetaProgramming) block
  else
    try block
    catch
      case e: Exception =>
        val dfErr = e match
          case e: IllegalArgumentException => DFError.Basic(ctName.value, e)
          case e: DFError                  => e
          case e                           => throw e
        if (dfc.ownerOption.isEmpty)
          exitWithError(dfErr.toString())
        dfc.logError(dfErr)

def exitWithError(msg: String): Nothing =
  System.err.println(msg)
  sys.exit(1)
