package DFiant.compiler
package printing
import ir.*
import DFiant.internals.*
import scala.collection.mutable
import analysis.*

protected trait AbstractPrinter:
  type TPrinter <: Printer
  given printer: TPrinter
  given getSet: MemberGetSet

trait Printer
    extends AbstractTypePrinter,
      AbstractTokenPrinter,
      AbstractValPrinter,
      AbstractOwnerPrinter:
  enum CommentConnDir derives CanEqual:
    case Off, Inline, EOL
  val commentConnDir: CommentConnDir
  def csAssignmentOp: String
  def csNBAssignmentOp: String
  def csConnectionOp: String
  def csLateConnectionOp: String
  def csLateConnectionSep: String
  def csLazyConnectionOp: String
  val normalizeLateConnection: Boolean
  val normalizeConnection: Boolean
  final def csDFNetOp(net: DFNet): String = net.op match
    case DFNet.Op.Assignment   => csAssignmentOp
    case DFNet.Op.NBAssignment => csNBAssignmentOp
    case DFNet.Op.Connection =>
      if (net.lateConstruction) csLateConnectionOp
      else csConnectionOp
    case DFNet.Op.LazyConnection => csLazyConnectionOp
  def csInternalViaPortRef(dfValRef: DFNet.Ref): String
  def csEndOfStatement: String
  final def csDFNet(net: DFNet): String =
    // True if the net needs to be shown in a swapped order.
    // Normalized late connections always have the internal port on the LHS.
    // Normalized connections always have the receiver port on the LHS.
    val swapLR = net match
      // swapped if the net is a late construction and the RHS is the internal port
      case _ if net.lateConstruction =>
        normalizeLateConnection && net.rhsRef.get.isSameOwnerDesignAs(net)
      // swapped if the net is a regular connection and the RHS is receiver
      case DFNet.Connection(_, _, swapped) =>
        swapped && normalizeConnection
      case _ => false
    val directionStr =
      net.lhsRef.get match
        case dfIfc: DFInterfaceOwner => "<->"
        case dfVal: DFVal =>
          if (dfVal.getConnectionTo.contains(net) ^ swapLR) "<--"
          else "-->"
    val opStr = commentConnDir match
      case CommentConnDir.Inline
          if (!normalizeConnection || net.lateConstruction) && !net.isAssignment =>
        s"${csDFNetOp(net)}${csCommentInline(directionStr)}"
      case _ => csDFNetOp(net)

    val (lhsRef, rhsRef) = if (swapLR) (net.rhsRef, net.lhsRef) else (net.lhsRef, net.rhsRef)
    val leftStr = if (net.lateConstruction) csInternalViaPortRef(lhsRef) else lhsRef.refCodeString
    val rightStr = rhsRef.refCodeString
    s"$leftStr $opStr $rightStr"
  end csDFNet
  def csTimeUnit(time: Time): String = s"${time.usec}.us"
  def csFreqUnit(freq: Freq): String = s"${freq.hertz}.Hz"
  def csRatioUnit(ratio: Ratio): String = s"${ratio.value}"
  def csTimer(timer: Timer): String
  def csCommentInline(comment: String): String
  def csCommentEOL(comment: String): String
  final def csDFMember(member: DFMember): String = member match
    case dfVal: DFVal.CanBeExpr if dfVal.isAnonymous => csDFValExpr(dfVal)
    case dfVal: DFVal                                => csDFValNamed(dfVal)
    case net: DFNet                                  => csDFNet(net)
    case design: DFDesignBlock                       => csDFDesignBlockInst(design)
    case ab: AlwaysBlock                             => csAlwaysBlock(ab)
    case domain: DomainBlock                         => csDomainBlock(domain)
    case timer: Timer                                => csTimer(timer)
    case _                                           => ???
  final def csDB(db: DB): String =
    import db.getSet
    val uniqueDesigns = mutable.Set.empty[String]
    val codeStringList = db.designMemberList.collect {
      case (block: DFDesignBlock, members) if !uniqueDesigns.contains(block.dclName) =>
        uniqueDesigns += block.dclName
        csDFDesignBlockDcl(block)
    }
    s"${csGlobalTypeDcls.emptyOr(v => s"$v\n\n")}${codeStringList.mkString("\n\n")}"
  end csDB
end Printer

class DFPrinter(using val getSet: MemberGetSet)
    extends Printer,
      DFTypePrinter,
      DFTokenPrinter,
      DFValPrinter,
      DFOwnerPrinter:
  type TPrinter = DFPrinter
  given printer: TPrinter = this
  val commentConnDir: CommentConnDir = CommentConnDir.Inline
  def csAssignmentOp: String = ":="
  def csNBAssignmentOp: String = ":="
  def csConnectionOp: String = "<>"
  def csLateConnectionOp: String = "<>"
  def csLateConnectionSep: String = ""
  def csLazyConnectionOp: String = "`<LZ>`"
  val normalizeLateConnection: Boolean = true
  val normalizeConnection: Boolean = true
  // to remove ambiguity in referencing a port inside a class instance we add `this.` as prefix
  def csInternalViaPortRef(dfValRef: DFNet.Ref): String = s"this.${dfValRef.refCodeString}"
  def csCommentInline(comment: String): String =
    if (comment.contains('\n'))
      s"""/*
         |${comment.indent}
         |*/""".stripMargin
    else s"/*$comment*/"
  def csEndOfStatement: String = ""
  def csCommentEOL(comment: String): String = s"// $comment"
  def csTimer(timer: Timer): String =
    val timerBody = timer match
      case p: Timer.Periodic =>
        (p.triggerRef.get, p.periodOpt) match
          case (DFMember.Empty, None)         => "Timer()"
          case (DFMember.Empty, Some(period)) => s"Timer(${csTimeUnit(period)})"
          case (trigger: DFVal, None) =>
            s"Timer(${p.triggerRef.refCodeString})"
          case (trigger: DFVal, Some(period)) =>
            s"Timer(${p.triggerRef.refCodeString},${csTimeUnit(period)})"
          case _ => ??? // impossible
      case f: Timer.Func =>
        val argStr = f.arg match
          case r: Ratio => csRatioUnit(r)
          case t: Time  => csTimeUnit(t)
        s"${f.sourceRef.refCodeString} ${f.op} $argStr"
    if (timer.isAnonymous) timerBody else s"val ${timer.name} = $timerBody"
  end csTimer
end DFPrinter

extension (member: DFMember)(using printer: Printer)
  def codeString: String =
    printer.csDFMember(member)
extension (db: DB)(using printer: Printer)
  def codeString: String =
    printer.csDB(db)
extension (dfType: DFType)(using printer: DFTypePrinter)
  def codeString: String =
    printer.csDFType(dfType)
extension (token: DFTokenAny)(using printer: DFTokenPrinter)
  def codeString: String =
    printer.csDFToken(token)

def DefaultPrinter(using MemberGetSet): Printer = new DFPrinter
