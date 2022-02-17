package DFiant.compiler.stages.vhdl
import DFiant.compiler.printing.*
import DFiant.compiler.ir.*
import DFiant.compiler.analysis.*
import DFiant.internals.*

class RTPrinter(using val getSet: MemberGetSet)
    extends Printer,
      RTTypePrinter,
      RTTokenPrinter,
      RTValPrinter,
      RTOwnerPrinter:
  type TPrinter = RTPrinter
  given printer: TPrinter = this
  def unsupported: Nothing = throw new IllegalArgumentException(
    "Unsupported member for this RTPrinter."
  )
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
end RTPrinter
