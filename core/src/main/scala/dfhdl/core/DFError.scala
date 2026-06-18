package dfhdl.core
import dfhdl.compiler.ir
import dfhdl.internals.*

import scala.annotation.targetName
import dfhdl.options.OnError

sealed trait LogEvent derives CanEqual:
  val dfMsg: String

sealed abstract class DFError(
    val dfMsg: String
) extends Exception(dfMsg), LogEvent

object DFError:
  class Basic(
      val opName: String,
      val iae: IllegalArgumentException
  )(using dfc: DFC)
      extends DFError(iae.getMessage):
    import dfc.getSet
    val ownerOptionCurrent = dfc.ownerOption.map(_.asIR)
    val dfcName = dfc.name
    lazy val designName = ownerOptionCurrent match
      case Some(owner) => owner.getThisOrOwnerDesign.getFullName
      case None        => ""
    lazy val fullName =
      if (dfc.isAnonymous) designName
      else if (designName.nonEmpty) s"$designName.${dfcName}"
      else dfcName
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
    inline def asFE[T <: DFTypeAny]: T = new DFType(dfErr).asInstanceOf[T]
    inline def asValOf[T <: DFTypeAny]: DFValOf[T] = DFVal[T, ModifierAny, DFError](dfErr)
    inline def asVal[T <: DFTypeAny, M <: ModifierAny]: DFVal[T, M] = DFVal[T, M, DFError](dfErr)
    inline def asOwner: DFOwnerAny = DFOwner[ir.DFOwner](dfErr)
end DFError

class DFWarning(
    val opName: String,
    val dfMsg: String
)(using dfc: DFC)
    extends LogEvent derives CanEqual:
  import dfc.getSet
  // TODO: revisit this in the future, since maybe laziness will get a wrong position
  // ---
  // Names are computed lazily: a warning may be constructed while its design is
  // still mid-elaboration (its `designInstCache` not yet set). Deferring the
  // lookup to `toString` time -- which happens at the end of top-level
  // elaboration -- ensures all design inst caches are populated.
  lazy val designName = dfc.ownerOption match
    case Some(owner) => owner.asIR.getThisOrOwnerDesign.getFullName
    case None        => ""
  lazy val fullName =
    if (dfc.isAnonymous) designName
    else if (designName.nonEmpty) s"$designName.${dfc.name}"
    else dfc.name
  val position = dfc.position
  override def toString: String =
    s"""|DFiant HDL elaboration warning!
        |Position:  ${position}
        |Hierarchy: ${fullName}
        |Operation: `${opName}`
        |Message:   ${dfMsg}""".stripMargin
end DFWarning

class Logger:
  private[Logger] var events: List[LogEvent] = Nil
  def logEvent(event: LogEvent): Unit = events = event :: events
  def injectEvents(fromLogger: Logger): Unit = events = fromLogger.events ++ events
  def injectEvents(newEvents: List[LogEvent]): Unit = events = events ++ newEvents
  def getErrors: List[DFError] = events.reverse.collect { case e: DFError => e }
  def getWarnings: List[DFWarning] = events.reverse.collect { case w: DFWarning => w }
  def getEvents: List[LogEvent] = events.reverse
  def clearEvents(): Unit = events = Nil

def trydfSpecific[T](
    block: => T
)(finale: DFError => T)(using dfc: DFC, ctName: CTName): T =
  if (dfc.inMetaProgramming || !dfc.elaborationOptions.trapErrors) block
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
        dfc.logEvent(dfErr)
        finale(dfErr)

@targetName("tryDFType")
@metaContextForward(0)
def trydf[T <: DFTypeAny](block: => T)(using DFC, CTName): T =
  trydfSpecific(block)(dfErr => new DFTypeAny(dfErr).asInstanceOf[T])

@targetName("tryDFVal")
@metaContextForward(0)
def trydf[V <: DFValAny](block: => V)(using DFC, CTName): V =
  trydfSpecific(block)(_.asVal[DFTypeAny, ModifierAny].asInstanceOf[V])

@targetName("tryDFNet")
@metaContextForward(0)
def trydf(block: => Unit)(using DFC, CTName): Unit =
  trydfSpecific(block)(_ => ())

@targetName("tryDFOwner")
@metaContextForward(0)
def trydf[V <: DFOwnerAny](block: => V)(using DFC, CTName): V =
  trydfSpecific(block)(_.asOwner.asInstanceOf[V])

def exitWithError(msg: String)(using DFC): Nothing =
  dfc.elaborationOptions.onError match
    case OnError.Exit =>
      println(msg)
      sys.exit(1)
    case _ =>
      throw new IllegalArgumentException(s"Elaboration errors found!\n$msg")
