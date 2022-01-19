package DFiant.compiler
package printing
import ir.*
import scala.collection.mutable
import analysis.*

protected trait AbstractPrinter:
  given printer: Printer
  given getSet: MemberGetSet

class Printer(using val getSet: MemberGetSet)
    extends DFTypePrinter,
      DFTokenPrinter,
      DFValPrinter,
      DFOwnerPrinter:
  given printer: Printer = this
  def csDFNet(net: DFNet): String =
    // to remove ambiguity in referencing a port inside a class instance we add `this.` as prefix
    val lhsThis =
      if (net.hasLateConstruction && net.toRef.get.isSameOwnerDesignAs(net)) "this."
      else ""
    val rhsThis =
      if (net.hasLateConstruction && net.fromRef.get.isSameOwnerDesignAs(net)) "this."
      else ""
    import net.*
    val opStr = op match
      case DFNet.Op.Assignment     => ":="
      case DFNet.Op.Connection     => "<>"
      case DFNet.Op.LazyConnection => "`<LZ>`"
    s"$lhsThis${toRef.refCodeString} $opStr $rhsThis${fromRef.refCodeString}"

  def csDFMember(member: DFMember): String = member match
    case dfVal: DFVal          => csDFVal(dfVal, None)
    case net: DFNet            => csDFNet(net)
    case design: DFDesignBlock => csDFDesignBlockInst(design)
    case _                     => ???
  def csDB(db: DB): String =
    import db.getSet
    val uniqueDesigns = mutable.Set.empty[String]
    val globalDcls =
      db.getGlobalNamedDFTypes.toList
        .sortBy(_.getName) // we sort the declarations by name, to have compilation consistency
        .map(printer.csNamedDFTypeDcl)
        .mkString("\n")
    val codeStringList = db.blockMemberList.flatMap {
//      case (DFDesign.Block.Internal(_,_,_,Some(_)), _) => None
//      case (d @ realtime.RTExternal.Block(), _) => None
      case (block: DFDesignBlock, members) if !uniqueDesigns.contains(block.dclName) =>
        uniqueDesigns += block.dclName
        Some(csDFDesignBlockDcl(block))
      case _ => None
    }
//    (globalEnumString ++ codeStringList).mkString(s"\n$EMPTY\n").formatted
    s"$globalDcls\n\n${codeStringList.mkString("\n\n")}"
  end csDB
end Printer

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

def DefaultPrinter(using MemberGetSet): Printer = new Printer
