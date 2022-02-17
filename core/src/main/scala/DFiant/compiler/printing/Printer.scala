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
  type TPrinter = Printer
  given printer: TPrinter = this
  val showNetDirection: Boolean = true
  def csDFNet(net: DFNet): String

  final def csDFMember(member: DFMember): String = member match
    case dfVal: DFVal.CanBeExpr if dfVal.isAnonymous => csDFValExpr(dfVal)
    case dfVal: DFVal                                => csDFValNamed(dfVal)
    case net: DFNet                                  => csDFNet(net)
    case design: DFDesignBlock                       => csDFDesignBlockInst(design)
    case ab: AlwaysBlock                             => csAlwaysBlock(ab)
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
  def csDFNet(net: DFNet): String =
    // true if the net is a late construction and the RHS is the internal port,
    // so we need to swap positions since we always present the internal on the left side.
    val swapLR = net.lateConstruction && net.rhsRef.get.isSameOwnerDesignAs(net)
    // to remove ambiguity in referencing a port inside a class instance we add `this.` as prefix
    val lhsThis =
      if (swapLR || net.lateConstruction && net.lhsRef.get.isSameOwnerDesignAs(net)) "this."
      else ""
    import net.*
    val directionStr =
      if (showNetDirection)
        net.lhsRef.get match
          case dfIfc: DFInterfaceOwner => "/*<->*/"
          case dfVal: DFVal =>
            if (dfVal.getConnectionTo.contains(net) ^ swapLR) "/*<--*/"
            else "/*-->*/"
      else ""
    val opStr = op match
      case DFNet.Op.Assignment     => ":="
      case DFNet.Op.Connection     => s"<>$directionStr"
      case DFNet.Op.LazyConnection => s"`<LZ>`$directionStr"

    val (leftStr, rightStr) =
      if (swapLR) (rhsRef.refCodeString, lhsRef.refCodeString)
      else (lhsRef.refCodeString, rhsRef.refCodeString)
    s"$lhsThis$leftStr $opStr $rightStr"
  end csDFNet
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
