package DFiant.compiler
package printing
import ir.*
import scala.collection.mutable
import analysis.*

protected trait AbstractPrinter:
  given printer: Printer

trait Printer
    extends DFTypePrinter,
      DFTokenPrinter,
      DFValPrinter,
      DFOwnerPrinter:
  given printer: Printer = this
  def csDFNet(net: DFNet)(using MemberGetSet): String =
    import net.*
    val opStr = op match
      case DFNet.Op.Assignment     => ":="
      case DFNet.Op.Connection     => "<>"
      case DFNet.Op.LazyConnection => "`<LZ>`"
    s"${toRef.refCodeString} $opStr ${fromRef.refCodeString}"

  def csDFMember(member: DFMember)(using MemberGetSet): String = member match
    case dfVal: DFVal   => csDFVal(dfVal, None)
    case net: DFNet     => csDFNet(net)
    case owner: DFOwner => csDFOwner(owner)
    case _              => ???
  def csDB(db: DB): String =
    import db.getSet
    val uniqueDesigns = mutable.Set.empty[String]
//    val globalEnumString = db.getGlobalEnumEntries.map(e => e.codeString)
    val codeStringList = db.blockMemberList.flatMap {
//      case (DFDesign.Block.Internal(_,_,_,Some(_)), _) => None
//      case (d @ realtime.RTExternal.Block(), _) => None
      case (block: DFDesignBlock, members)
          if !uniqueDesigns.contains(block.designType) =>
        uniqueDesigns += block.designType
        Some(csDFDesignBlockDcl(block))
      case _ => None
    }
//    (globalEnumString ++ codeStringList).mkString(s"\n$EMPTY\n").formatted
    codeStringList.mkString("\n\n")
  end csDB
end Printer

extension (member: DFMember)(using printer: Printer)
  def codeString(using MemberGetSet): String =
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

object DefaultPrinter extends Printer
