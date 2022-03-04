package DFiant.core
import DFiant.compiler.ir
import DFiant.compiler.ir.DFVal.ModifierAny
import DFiant.internals.*

import scala.annotation.targetName

sealed abstract class DFError(
    val dfMsg: String
) extends Exception(dfMsg)
    derives CanEqual

object DFError:
  final class Basic(
      val opName : String,
      val iae: IllegalArgumentException
  )(using dfc: DFC) extends DFError(iae.getMessage):
    import dfc.getSet
    val designName = dfc.owner.asIR.getThisOrOwnerDesign.getFullName
    val fullName =
      if (dfc.isAnonymous) designName
      else s"$designName.${dfc.name}"
    val position = dfc.position
    override def toString : String = 
      s"""|DFiant HDL elaboration error!
          |Position:  ${position}
          |Hierarchy: ${fullName}
          |Operation: `${opName}`
          |Message:   ${dfMsg}""".stripMargin
    
  final class Derived(from: DFError) extends DFError(from.dfMsg)

  extension (dfErr: DFError)
    def asNet: DFNet = new DFNet(dfErr)
    def asFE[T <: DFTypeAny]: T = DFType(dfErr).asInstanceOf[T]
    def asValOf[T <: DFTypeAny]: DFValOf[T] = new DFVal[T, ModifierAny](dfErr)
    def asVal[T <: DFTypeAny, M <: ModifierAny]: DFVal[T, M] = new DFVal[T, M](dfErr)
    def asTokenOf[T <: DFTypeAny]: DFToken[T] = new DFToken[T](dfErr)
end DFError

class Logger:
  private var errors: List[DFError] = Nil
  def logError(err: DFError): Unit =
    errors = err :: errors
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
def trydf[T <: DFTypeAny, M <: ModifierAny](block: => DFVal[T, M])(using dfc: DFC, ctName : CTName): DFVal[T, M] =
  try
    val ret = block
    import dfc.getSet
    val retIR = dfc.getSet.set(ret.asIR)(_.setMeta(_ => dfc.getMeta))
    retIR.asVal[T, M]
  catch
    case e: Exception =>
      val dfErr = e match
        case e: IllegalArgumentException => DFError.Basic(ctName.value, e)
        case e: DFError                  => e
      dfc.logError(dfErr)
      dfErr.asVal[T, M]

@targetName("tryDFNet")
def trydf(block: => DFNet)(using dfc: DFC, ctName : CTName): DFNet =
  try
    val ret = block
    import dfc.getSet
    val retIR = dfc.getSet.set(ret.asIR)(_.setMeta(_ => dfc.getMeta))
    retIR.asFE
  catch
    case e: Exception =>
      val dfErr = e match
        case e: IllegalArgumentException => DFError.Basic(ctName.value, e)
        case e: DFError                  => e
      dfc.logError(dfErr)
      dfErr.asNet

def exitWithError(msg : String): Unit =
  System.err.println(msg)
  sys.exit(1)