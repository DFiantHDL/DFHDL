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
import scala.annotation.targetName

object Ident:
  def unapply(alias: DFVal.Alias.AsIs)(using MemberGetSet): Option[DFVal] =
    if (alias.hasTagOf[DFVal.Alias.IdentTag.type])
      Some(alias.relValRef.get)
    else None

//A design parameter is an as-is alias that:
//1. has `DesignParamTag` tag
//TODO: This is not yet working. more complicated than initially thought.
//      Could be the best method is to remove the tag in the plugin.
//2. its values is not from another `DesignParamTag`-tagged value that has
//   belongs to the same design.
//
//Here is an example:
//```
//class Foo(arg: Bit <> CONST) extends DFDesign:
//  val x = Bit <> IN init arg
//class Bar(newArg: Bit <> CONST) extends Foo(arg)
//val barTop = Bar(1)
//```
//In this example we have both `arg` and `newArg` tagged with `DesignParamTag`.
//However, we only treat `newArg` as a design parameter and consider `arg`
//to be a local constant parameter.
//If we print `barTop` after elaboration we get the following:
//```
//class Bar(newArg: Bit <> CONST) extends DFDesign:
//  val arg: Bit <> CONST = newArg
//  val x = Bit <> IN init arg
//```
object DesignParam:
  def unapply(alias: DFVal.Alias.AsIs)(using MemberGetSet): Option[DFVal] =
    if (alias.hasTagOf[DFVal.Alias.DesignParamTag.type])
      val relVal = alias.relValRef.get
      // if (
      //   relVal.existsInComposedReadDeps { dep =>
      //     dep.hasTagOf[DFVal.Alias.DesignParamTag.type] &&
      //     dep.isSameOwnerDesignAs(alias)
      //   }
      // ) None
      Some(relVal)
    else None
end DesignParam

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

//not only `DFVal.Const` but all non-anonymous values that
//are known to be constant from their dependencies.
object DclConst:
  def unapply(dfVal: DFVal.CanBeExpr)(using
      MemberGetSet
  ): Boolean = !dfVal.isAnonymous && dfVal.isConst

object DclVar:
  def unapply(dcl: DFVal.Dcl)(using
      MemberGetSet
  ): Boolean = dcl.modifier match
    case Modifier.VAR => true
    case _            => false

object DclPort:
  def unapply(dcl: DFVal.Dcl)(using
      MemberGetSet
  ): Boolean = dcl.modifier match
    case Modifier.IN | Modifier.OUT | Modifier.INOUT => true
    case _                                           => false

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

extension (ref: DFRef.TwoWayAny)
  def originMember(using MemberGetSet): DFMember =
    getSet.designDB.originRefTable(ref)

extension (member: DFMember)
  def originMembers(using MemberGetSet): Set[DFMember] =
    getSet.designDB.originMemberTable.getOrElse(member, Set())

extension (dfVal: DFVal)
  def getPartialAliases(using MemberGetSet): Set[DFVal.Alias.Partial] =
    dfVal.originMembers.flatMap {
      case alias: DFVal.Alias.Partial => Some(alias)
      case _                          => None
    }
  def hasPrevAlias(using MemberGetSet): Boolean =
    dfVal.originMembers.exists {
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
  def getPortsByNameSelectors(using MemberGetSet): List[DFVal.PortByNameSelect] =
    dfVal match
      case dcl @ DclPort() =>
        getSet.designDB.portsByNameSelectors.getOrElse(dcl, Nil)
      case _ => Nil
  // search composed value read dependencies (without nets) if there is a value
  // that fits the given condition
  def existsInComposedReadDeps(cond: DFVal => Boolean)(using MemberGetSet): Boolean =
    dfVal.originMembers.view
      .collect { case dfVal: DFVal => dfVal }
      .exists(dfVal => cond(dfVal) || dfVal.existsInComposedReadDeps(cond))
  def getReadDeps(using MemberGetSet): Set[DFNet | DFVal] =
    val fromRefs: Set[DFNet | DFVal] = dfVal.originMembers.flatMap {
      case net: DFNet =>
        net match
          // ignoring receiver or if connecting to an OPEN
          case DFNet.Connection(toVal: DFVal, _, _) if toVal.isOpen || toVal == dfVal => None
          // ignoring receiver
          case DFNet.Assignment(toVal, _) if toVal == dfVal => None
          case _                                            => Some(net)
      case dfVal: DFVal => Some(dfVal)
      case _            => None
    }
    dfVal match
      // for ports we need to also account for by-name referencing
      case port @ DclPort() =>
        val designInst = port.getOwnerDesign
        designInst.originMembers.view
          .collect { case ps @ DFVal.PortByNameSelect.Of(p) if p == port => ps.getReadDeps }
          .flatten
          .toSet ++ fromRefs
      case _ => fromRefs
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
    val origins = member.originMembers

    val refOwner: Option[DFMember] =
      // search consuming references first
      origins.filter {
        case _: (DFVal.Alias.Partial | DFConditional.Block) => false
        case _                                              => true
      }.headOption
        // search aliasing references, as long as we don't go back to previous member
        // (aliasing can be used for both producing and consuming)
        .orElse {
          origins.collectFirst {
            case m if prevMember.isEmpty || prevMember.get != m => m
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
  def isBubble(using MemberGetSet): Boolean =
    dfVal match
      case c: DFVal.Const          => c.token.isBubble
      case f: DFVal.Func           => f.args.exists(_.get.isBubble)
      case a: DFVal.Alias.ApplyIdx => a.relValRef.get.isBubble || a.relIdx.get.isBubble
      case a: DFVal.Alias.Partial  => a.relValRef.get.isBubble
      case _                       => false
end extension

extension (refTW: DFNet.Ref)
  def isViaRef(using MemberGetSet): Boolean =
    refTW.originMember match
      case net: DFNet if net.isViaConnection =>
        refTW.get.stripPortSel.getOwner.isSameOwnerDesignAs(net)
      case _ => false

extension (origVal: DFVal)
  private def collectRelMembersRecur(includeOrigVal: Boolean)(using MemberGetSet): List[DFVal] =
    if (origVal.isAnonymous && !origVal.isGlobal || includeOrigVal)
      origVal :: origVal.getRefs.view.map(_.get).flatMap {
        case dfVal: DFVal => dfVal.collectRelMembersRecur(false)
        case _            => Nil
      }.toList
    else Nil
  @targetName("collectRelMembersDFVal")
  def collectRelMembers(includeOrigVal: Boolean)(using MemberGetSet): List[DFVal] =
    origVal.collectRelMembersRecur(includeOrigVal).reverse

extension (net: DFNet)
  @targetName("collectRelMembersDFNet")
  def collectRelMembers(using MemberGetSet): List[DFVal] =
    net match
      case DFNet(DFRef(lhs: DFVal), _, DFRef(rhs: DFVal), _, _, _) =>
        lhs.collectRelMembers(false) ++ rhs.collectRelMembers(false)
      case _ => Nil

extension (dfVal: DFVal)
  def getParamToken(using MemberGetSet): Option[DFTokenAny] =
    import DFToken.calcFuncOp
    import DFBits.Ops.sel
    dfVal match
      case const: DFVal.Const => Some(const.token)
      case func: DFVal.Func =>
        val args = func.args.flatMap(_.get.getParamToken)
        if (args.length != func.args.length) None
        else Some(calcFuncOp(func.op, args))
      case alias: DFVal.Alias =>
        alias.relValRef.get.getParamToken match
          case Some(relToken) =>
            alias match
              case alias: DFVal.Alias.AsIs => Some(relToken.as(alias.dfType))
              case alias: DFVal.Alias.ApplyRange =>
                Some(relToken.asInstanceOf[DFBits.Token].sel(alias.relBitHigh, alias.relBitLow))
              case alias: DFVal.Alias.ApplyIdx =>
                alias.relIdx.get.getParamToken match
                  case Some(relIdxToken) =>
                    (relToken.dfType: @unchecked) match
                      case _: DFVector => ???
                      case _: DFBits   => ???
                  case None => None
              case alias: DFVal.Alias.History     => None
              case alias: DFVal.Alias.SelectField => ???
          case None => None
      case _ => None
    end match
