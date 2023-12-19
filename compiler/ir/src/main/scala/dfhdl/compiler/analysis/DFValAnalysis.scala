package dfhdl.compiler
package analysis
import dfhdl.internals.*
import ir.*
import DFConditional.DFCaseBlock.Pattern
import DFVal.Modifier
import DFVal.Func.Op as FuncOp
import DFVal.Alias.History.Op as HistoryOp
import scala.annotation.tailrec
import scala.reflect.{ClassTag, classTag}
import DFDesignBlock.InstMode
import scala.util.boundary, boundary.break

object Ident:
  def unapply(alias: DFVal.Alias.AsIs)(using MemberGetSet): Option[DFVal] =
    if (alias.getTagOf[DFVal.Alias.IdentTag.type].isDefined)
      Some(alias.relValRef.get)
    else None

object OpaqueActual:
  def unapply(alias: DFVal.Alias.AsIs)(using MemberGetSet): Option[DFVal] =
    val relVal = alias.relValRef.get
    relVal.dfType match
      case dfType: DFOpaque if dfType.actualType == alias.dfType => Some(relVal)
      case _                                                     => None

object Bind:
  def unapply(alias: DFVal.Alias)(using MemberGetSet): Option[DFVal] =
    if (alias.getTagOf[Pattern.Bind.Tag.type].isDefined)
      Some(alias.relValRef.get)
    else None

object ClkEdge:
  def unapply(func: DFVal.Func)(using MemberGetSet): Option[(DFVal, ClkCfg.Edge)] =
    func.op match
      case FuncOp.rising if func.args.length == 1  => Some(func.args.head.get, ClkCfg.Edge.Rising)
      case FuncOp.falling if func.args.length == 1 => Some(func.args.head.get, ClkCfg.Edge.Falling)
      case _                                       => None

object RstActive:
  def unapply(dfVal: DFVal)(using MemberGetSet): Option[(DFVal, RstCfg.Active)] =
    boundary:
      dfVal match
        case func: DFVal.Func =>
          func.op match
            case FuncOp.=== | FuncOp.=!= =>
              val List(lhsRef, rhsRef) = func.args
              val (dcl, const) = (lhsRef.get, rhsRef.get) match
                case (dcl: DFVal.Dcl, const: DFVal.Const) if const.dfType == DFBit => (dcl, const)
                case (const: DFVal.Const, dcl: DFVal.Dcl) if const.dfType == DFBit => (dcl, const)
                case _                                                             => break(None)
              val DFBoolOrBit.Token(_, Some(value: Boolean)) = const.token: @unchecked
              val actualValue = func.op match
                case FuncOp.=== => value
                case _          => !value
              val active = if (actualValue) RstCfg.Active.High else RstCfg.Active.Low
              Some(dcl, active)
            case FuncOp.unary_! =>
              val relVal = func.args.head.get
              Some(relVal, RstCfg.Active.Low)
            case _ => None
        case dcl: DFVal.Dcl => Some(dcl, RstCfg.Active.High)
        case _              => None
end RstActive

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

object PortOfDesignDef:
  def unapply(dcl: DFVal.Dcl)(using
      MemberGetSet
  ): Option[(Modifier.IN.type | Modifier.OUT.type, DFDesignBlock)] =
    dcl.modifier match
      case mod: (Modifier.IN.type | Modifier.OUT.type) =>
        val design = dcl.getOwnerDesign
        if (design.instMode == InstMode.Def) Some(mod, design)
        else None
      case _ => None

extension (dcl: DFVal.Dcl)
  def externalInit: Option[List[DFTokenAny]] = dcl.getTagOf[ExternalInit].map(_.tokenSeq)

private val netClassTag = classTag[DFNet]
private val aliasPartClassTag = classTag[DFVal.Alias.Partial]
private val nonConsumingRefs: Set[ClassTag[?]] = Set(
  aliasPartClassTag,
  classTag[DFConditional.DFIfElseBlock],
  classTag[DFConditional.DFCaseBlock]
)

extension (member: DFMember)
  def originRefs(using MemberGetSet): Set[DFRefAny] =
    getSet.designDB.memberTable
      .getOrElse(member, Set.empty).collect { case r: DFRef.TwoWayAny => r.originRef }

extension (dfVal: DFVal)
  def getPartialAliases(using MemberGetSet): Set[DFVal.Alias.Partial] =
    dfVal.originRefs.flatMap {
      case r if r.refType equals aliasPartClassTag =>
        r.get match
          case alias: DFVal.Alias.Partial => Some(alias)
          case _                          => None
      case _ => None
    }
  def hasPrevAlias(using MemberGetSet): Boolean =
    val refs = dfVal.originRefs
    refs.exists { r =>
      r.get match
        case history: DFVal.Alias.History if (history.op == DFVal.Alias.History.Op.Prev) =>
          true
        case alias: DFVal.Alias =>
          alias.hasPrevAlias
        case _ => false
    }
  end hasPrevAlias
  def getConnectionTo(using MemberGetSet): Option[DFNet] =
    getSet.designDB.connectionTable.getNets(dfVal).headOption
  def getConnectionsFrom(using MemberGetSet): Set[DFNet] =
    getSet.designDB.connectionTableInverted.getOrElse(dfVal, Set())
  def getAssignmentsTo(using MemberGetSet): Set[DFVal] =
    getSet.designDB.assignmentsTable.getOrElse(dfVal, Set())
  def getAssignmentsFrom(using MemberGetSet): Set[DFVal] =
    getSet.designDB.assignmentsTableInverted.getOrElse(dfVal, Set())
  def getReadDeps(using MemberGetSet): Set[DFNet | DFVal] =
    val refs = dfVal.originRefs
    refs.flatMap { r =>
      r.get match
        case net: DFNet =>
          net match
            // ignoring
            case DFNet.Connection(toVal: DFVal, _, _) if toVal == dfVal => None
            case DFNet.Assignment(toVal, _) if toVal == dfVal           => None
            case _                                                      => Some(net)
        case dfVal: DFVal => Some(dfVal)
        case _            => None
    }
  end getReadDeps

  @tailrec private def flatName(member: DFVal, suffix: String)(using MemberGetSet): String =
    member match
      case named if !named.isAnonymous => s"${member.getName}$suffix"
      case alias: DFVal.Alias.Partial =>
        val relVal = alias.relValRef.get
        val newSuffix = alias match
          case _: DFVal.Alias.AsIs => suffix
          case applyIdx: DFVal.Alias.ApplyIdx =>
            applyIdx match
              case DFVal.Alias.ApplyIdx.Const(i) =>
                val maxValue = relVal.dfType match
                  case vector: DFVector => vector.cellDims.head - 1
                  case bits: DFBits     => bits.width - 1
                  case _                => ???
                s"_${i.toPaddedString(maxValue)}"
              case _ => "_sel"
          case applyRange: DFVal.Alias.ApplyRange =>
            s"_${applyRange.relBitHigh.toPaddedString(applyRange.width - 1)}_${applyRange.relBitLow.toPaddedString(applyRange.width - 1)}"
          case selectField: DFVal.Alias.SelectField => s"_${selectField.fieldName}"
        flatName(relVal, s"$newSuffix$suffix")
      case _ => s"${member.getName}$suffix"

  // returns the name if the value is named, or the flat representation using
  // suffixes to differential between partial field selection or index application
  def flatName(using MemberGetSet): String = flatName(dfVal, "")

  private def partName(member: DFVal)(using MemberGetSet): String = s"${member.flatName}_part"

  @tailrec private def suggestName(
      member: DFVal,
      prevMember: Option[DFVal] = None
  )(using MemberGetSet): Option[String] =
    val refs = member.originRefs

    val refOwner: Option[DFMember] = refs
      // search consuming references first
      .collectFirst { case r if !nonConsumingRefs.contains(r.refType) => r.get }
      // search aliasing references, as long as we don't go back to previous member
      // (aliasing can be used for both producing and consuming)
      .orElse {
        refs.collectFirst {
          case r if prevMember.isEmpty || prevMember.get != r.get => r.get
        }
      }
    refOwner match
      // name from assignment destination
      case Some(DFNet.Assignment(toVal, _)) => Some(partName(toVal))
      // name from connection destination
      case Some(DFNet.Connection(toVal: DFVal, _, _)) => Some(partName(toVal))
      // name from a named value which was referenced by an alias
      case Some(value: DFVal) if !value.isAnonymous => Some(partName(value))
      // found an (anonymous) value -> checking suggestion for it
      case Some(value: DFVal) => suggestName(value, Some(value))
      // no named source found
      case _ => None
  end suggestName
  def suggestName(using MemberGetSet): Option[String] = suggestName(dfVal)
end extension

extension (refTW: DFRef.TwoWayAny)
  def isViaRef(using MemberGetSet): Boolean =
    refTW.originRef.get match
      case net: DFNet if net.isViaConnection =>
        refTW.get.getOwner.isSameOwnerDesignAs(net)
      case _ => false

extension (net: DFNet)
  private def collectRelMembersRecur(dfVal: DFVal)(using MemberGetSet): List[DFVal] =
    if (dfVal.isAnonymous)
      dfVal :: dfVal.getRefs.view.map(_.get).flatMap {
        case dfVal: DFVal => collectRelMembersRecur(dfVal)
        case _            => Nil
      }.toList
    else Nil
  def collectRelMembers(using MemberGetSet): List[DFVal] =
    net match
      case DFNet(DFRef(lhs: DFVal), _, DFRef(rhs: DFVal), _, _, _) =>
        collectRelMembersRecur(lhs).reverse ++ collectRelMembersRecur(rhs).reverse
      case _ => Nil
