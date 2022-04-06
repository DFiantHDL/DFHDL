package DFiant.compiler
package analysis
import DFiant.internals.*
import ir.*
import DFConditional.DFCaseBlock.Pattern
import DFVal.Modifier

object Ident:
  def unapply(alias: ir.DFVal.Alias.AsIs)(using
      MemberGetSet
  ): Option[ir.DFVal] =
    val relVal = alias.relValRef.get
    if (alias.dfType == relVal.dfType) Some(relVal)
    else None

object Bind:
  def unapply(alias: ir.DFVal.Alias)(using
      MemberGetSet
  ): Option[ir.DFVal] =
    if (alias.getTagOf[Pattern.Bind.Tag.type].isDefined)
      Some(alias.relValRef.get)
    else None

object DclVar:
  def unapply(dcl: DFVal.Dcl)(using
      MemberGetSet
  ): Boolean = dcl.modifier match
    case Modifier.VAR => true
    case _            => false

object DclIn:
  def unapply(dcl: DFVal.Dcl)(using
      MemberGetSet
  ): Boolean = dcl.modifier match
    case Modifier.IN => true
    case _           => false

object DclOut:
  def unapply(dcl: DFVal.Dcl)(using
      MemberGetSet
  ): Boolean = dcl.modifier match
    case Modifier.OUT => true
    case _            => false

extension (dcl: DFVal.Dcl)
  def externalInit: Option[List[DFTokenAny]] = dcl.getTagOf[ExternalInit].map(_.tokenSeq)

extension (dfVal: DFVal)
  def hasPrevAlias(using MemberGetSet): Boolean =
    val refs = getSet.designDB.memberTable(dfVal)
    refs.foreach {
      case DFRef.TwoWay(originRef) =>
        originRef.get match
          case history: DFVal.Alias.History if (history.op == DFVal.Alias.History.Op.Prev) =>
            return true
          case alias: DFVal.Alias =>
            alias.hasPrevAlias
          case _ => // false
      case _ =>
    }
    false
  end hasPrevAlias
  def getConnectionTo(using MemberGetSet): Option[DFNet] =
    dfVal.dealias.flatMap(getSet.designDB.connectionTable.get)
  def getConnectionsFrom(using MemberGetSet): List[DFNet] =
    getSet.designDB.connectionTableInverted.getOrElse(dfVal, Nil)
  def getAssignmentsTo(using MemberGetSet): Set[DFVal] =
    getSet.designDB.assignmentsTable.getOrElse(dfVal, Set())
  def getAssignmentsFrom(using MemberGetSet): Set[DFVal] =
    getSet.designDB.assignmentsTableInverted.getOrElse(dfVal, Set())
  def getReadDeps(using MemberGetSet): Set[DFNet | DFVal] =
    val refs = getSet.designDB.memberTable(dfVal)
    refs.flatMap {
      case DFRef.TwoWay(originRef) =>
        originRef.get match
          case net: DFNet =>
            net match
              // ignoring
              case DFNet.Connection(toVal: DFVal, _, _) if toVal == dfVal => None
              case DFNet.Assignment(toVal, _) if toVal == dfVal           => None
              case _                                                      => Some(net)
          case alias: DFVal.Alias.ModPropagator => alias.getReadDeps
          case dfVal: DFVal                     => Some(dfVal)
          case _                                => None
      case _ => None
    }
  end getReadDeps
end extension
