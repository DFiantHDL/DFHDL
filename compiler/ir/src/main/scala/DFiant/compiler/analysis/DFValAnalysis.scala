package DFiant.compiler
package analysis
import DFiant.internals.*
import ir.*
import DFConditional.DFCaseBlock.Pattern
import DFVal.Modifier

import scala.annotation.tailrec

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

//  private def partName(member: DFVal) = s"${member.name}_part"
//  @tailrec private def suggestName(
//      member: DFVal,
//      prevMember: Option[DFVal] = None
//  )(using MemberGetSet): String =
//    val refs = getSet.designDB.memberTable(member).map(r => (r, r.refType))
//    val refOwner: Option[DFMember] = refs
//      // search consuming references first
//      .collectFirst { case (r: DFRefAny, _: DFAny.Ref.ConsumeFrom.Type) =>
//        r.owner.get
//      }
//      // search aliasing references, as long as we don't go back to previous member
//      // (aliasing can be used for both producing and consuming)
//      .orElse {
//        refs.collectFirst {
//          case (r: DFRefAny, _: DFAny.Alias.RelValRef.Type)
//              if prevMember.isEmpty || prevMember.get != r.owner.get =>
//            r.owner.get
//        }
//      }
//    refOwner match
//      // name from assignment destination
//      case Some(DFNet.Assignment(toVal, _)) => partName(toVal)
//      // name from connection destination
//      case Some(DFNet.Connection(toVal: DFVal, _)) => partName(toVal)
//      // name from a named value which was referenced by an alias
//      case Some(value: DFVal) if !value.isAnonymous => partName(value)
//      // found an (anonymous) value -> checking suggestion for it
//      case Some(value: DFVal) => suggestName(value, Some(value))
//      // no named source found -> relying on the default anonymous naming
//      case _ => member.name
//  end suggestName
//  def suggestName(using MemberGetSet): String = suggestName(dfVal)
//  def getNameOrSuggestion(using MemberGetSet): String =
//    if (dfVal.isAnonymous) dfVal.suggestName else dfVal.name
end extension
