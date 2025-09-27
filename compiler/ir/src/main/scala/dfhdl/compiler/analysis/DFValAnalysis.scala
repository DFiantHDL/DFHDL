package dfhdl.compiler
package analysis
import dfhdl.internals.*
import ir.*
import DFConditional.DFCaseBlock.Pattern
import DFVal.Modifier
import DFVal.Func.Op as FuncOp
import DFVal.Alias.History.Op as HistoryOp
import scala.annotation.tailrec
import DFDesignBlock.InstMode
import scala.util.boundary, boundary.break
import scala.annotation.targetName
import scala.collection.immutable.ListMap

object IteratorDcl:
  def unapply(dcl: DFVal.Dcl)(using MemberGetSet): Boolean =
    dcl.hasTagOf[IteratorTag]

object Ident:
  def unapply(alias: DFVal.Alias.AsIs)(using MemberGetSet): Option[DFVal] =
    if (alias.hasTagOf[IdentTag]) Some(alias.relValRef.get)
    else None

object StrippedPortByNameSelect:
  def unapply(dfVal: DFVal)(using MemberGetSet): Option[DFVal] = Some(dfVal.stripPortSel)

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
// object DesignParam:
//   def unapply(alias: DFVal.Alias.AsIs)(using MemberGetSet): Option[DFVal] =
//     if (alias.hasTagOf[DFVal.Alias.DesignParamTag])
//       val relVal = alias.relValRef.get
//       // if (
//       //   relVal.existsInComposedReadDeps { dep =>
//       //     dep.hasTagOf[DFVal.Alias.DesignParamTag] &&
//       //     dep.isSameOwnerDesignAs(alias)
//       //   }
//       // ) None
//       Some(relVal)
//     else None
// end DesignParam

//extract the design param from its default value
object DefaultOfDesignParam:
  def unapply(dfVal: DFVal)(using MemberGetSet): Option[DFVal.DesignParam] =
    dfVal.originMembers.collectFirst {
      case dp: DFVal.DesignParam if dp.defaultRef.get == dfVal => dp
    }

object OpaqueActual:
  def unapply(alias: DFVal.Alias.AsIs)(using MemberGetSet): Option[DFVal] =
    val relVal = alias.relValRef.get
    relVal.dfType match
      case dfType: DFOpaque if dfType.actualType equals alias.dfType => Some(relVal)
      case _                                                         => None

object AsOpaque:
  def unapply(alias: DFVal.Alias.AsIs)(using MemberGetSet): Option[DFVal] =
    val relVal = alias.relValRef.get
    alias.dfType match
      case dfType: DFOpaque if dfType.actualType equals relVal.dfType => Some(relVal)
      case _                                                          => None

object Bind:
  def unapply(alias: DFVal.Alias)(using MemberGetSet): Option[DFVal] =
    if (alias.getTagOf[BindTag].isDefined)
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
                case (dcl: (DFVal.Dcl | DFVal.PortByNameSelect), const: DFVal.Const)
                    if const.dfType equals DFBit =>
                  (dcl, const)
                case (const: DFVal.Const, dcl: (DFVal.Dcl | DFVal.PortByNameSelect))
                    if const.dfType equals DFBit =>
                  (dcl, const)
                case _ => break(None)
              val value = const.data.asInstanceOf[Option[Boolean]].get
              val actualValue = func.op match
                case FuncOp.=== => value
                case _          => !value
              val active = if (actualValue) RstCfg.Active.High else RstCfg.Active.Low
              Some(dcl, active)
            case FuncOp.unary_! =>
              val relVal = func.args.head.get
              Some(relVal, RstCfg.Active.Low)
            case _ => None
        case dcl: (DFVal.Dcl | DFVal.PortByNameSelect) => Some(dcl, RstCfg.Active.High)
        case _                                         => None
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
  ): Boolean = dcl.modifier.dir match
    case Modifier.VAR => true
    case _            => false

object DclPort:
  def unapply(dcl: DFVal.Dcl)(using
      MemberGetSet
  ): Boolean = dcl.modifier.dir match
    case Modifier.IN | Modifier.OUT | Modifier.INOUT => true
    case _                                           => false

object DclIn:
  def unapply(dcl: DFVal.Dcl)(using
      MemberGetSet
  ): Boolean = dcl.modifier.dir match
    case Modifier.IN => true
    case _           => false

object DclOut:
  def unapply(dcl: DFVal.Dcl)(using
      MemberGetSet
  ): Boolean = dcl.modifier.dir match
    case Modifier.OUT => true
    case _            => false

object PortOfDesignDef:
  def unapply(dcl: DFVal.Dcl)(using
      MemberGetSet
  ): Option[(Modifier.IN.type | Modifier.OUT.type, DFDesignBlock)] =
    dcl.modifier.dir match
      case mod: (Modifier.IN.type | Modifier.OUT.type) =>
        val design = dcl.getOwnerDesign
        if (design.instMode == InstMode.Def) Some(mod, design)
        else None
      case _ => None

object InitialValueOf:
  def unapply(initVal: DFVal)(using MemberGetSet): Option[DFVal.Dcl] =
    initVal.originMembersNoTypeRef.collectFirst {
      case dcl: DFVal.Dcl if dcl.initRefList.map(_.get).contains(initVal) => dcl
    }

object BlockRamVar:
  def unapply(dfVal: DFVal)(using MemberGetSet): Boolean = dfVal.dfType match
    case dfType: DFVector =>
      dfVal match
        case DclVar() =>
          // if the var only has index accesses, then it's a block-ram access.
          dfVal.getReadDeps.forall {
            case applyIdx: DFVal.Alias.ApplyIdx => true
            case _                              => false
          }
        case _ => false
    case _ => false

extension (ref: DFRef.TwoWayAny)
  def originMember(using MemberGetSet): DFMember =
    getSet.getOrigin(ref)

extension (member: DFMember)
  def originMembers(using MemberGetSet): Set[DFMember] =
    getSet.designDB.originMemberTable.getOrElse(member, Set())
  def originMembersNoTypeRef(using MemberGetSet): Set[DFMember] =
    getSet.designDB.originMemberTableNoTypeRef.getOrElse(member, Set())

extension (dfVal: DFVal)
  def getPartialAliases(using MemberGetSet): Set[DFVal.Alias.Partial] =
    dfVal.originMembers.flatMap {
      case alias: DFVal.Alias.Partial => Some(alias)
      case _                          => None
    }
  // def hasPrevAlias(using MemberGetSet): Boolean =
  //   dfVal.originMembers.exists {
  //     case history: DFVal.Alias.History if history.op == DFVal.Alias.History.Op.State =>
  //       ??? //TODO: if we need this, should this be `hasStateAlias?
  //     case alias: DFVal.Alias =>
  //       alias.hasPrevAlias
  //     case _ => false
  //   }
  // end hasPrevAlias
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
  def getReadDeps(using MemberGetSet): Set[TextOut | DFNet | DFVal | DFConditional.Block] =
    val fromRefs: Set[TextOut | DFNet | DFVal | DFConditional.Block] =
      dfVal.originMembersNoTypeRef.flatMap {
        case net: DFNet =>
          net match
            // ignoring receiver or if connecting to an OPEN
            case DFNet.Connection(toVal = toVal: DFVal) if toVal.isOpen || toVal == dfVal =>
              None
            // ignoring receiver
            case DFNet.Assignment(toVal = toVal) if toVal == dfVal => None
            case _                                                 => Some(net)
        case dfVal: DFVal                                                        => Some(dfVal)
        case guardBlock: DFConditional.Block if guardBlock.guardRef.get == dfVal => Some(guardBlock)
        case textOut: TextOut                                                    => Some(textOut)
        case _                                                                   => None
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
  def isReferencedByAnyDcl(using MemberGetSet): Boolean =
    dfVal.originMembers.view.exists {
      case _: DFVal.Dcl => true
      case DclConst()   => true
      case dfVal: DFVal => dfVal.isReferencedByAnyDcl
      case _            => false
    }

  @tailrec private def flatName(member: DFVal, suffix: String)(using MemberGetSet): String =
    member match
      case named if !named.isAnonymous => s"${member.getName}$suffix"
      case alias: DFVal.Alias.Partial  =>
        val relVal = alias.relValRef.get
        val newSuffix = alias match
          case _: DFVal.Alias.AsIs            => suffix
          case applyIdx: DFVal.Alias.ApplyIdx =>
            applyIdx.relIdx.get match
              case DFVal.Alias.ApplyIdx.ConstIdx(i) =>
                val maxValue = relVal.dfType match
                  case vector: DFVector => vector.length - 1
                  case bits: DFBits     => bits.width - 1
                  case _                => ???
                s"_${i.toPaddedString(maxValue)}"
              case _ => "_sel"
          case applyRange: DFVal.Alias.ApplyRange =>
            (applyRange.dfType: @unchecked) match
              case DFBits(_) =>
                val idxHigh = applyRange.idxHighRef.getInt.toPaddedString(applyRange.width - 1)
                val idxLow = applyRange.idxLowRef.getInt.toPaddedString(applyRange.width - 1)
                s"_${idxHigh}_${idxLow}"
              case dfType: DFVector =>
                val idxHigh = applyRange.idxHighRef.getInt.toPaddedString(dfType.length - 1)
                val idxLow = applyRange.idxLowRef.getInt.toPaddedString(dfType.length - 1)
                s"_${idxLow}_${idxHigh}"
          case selectField: DFVal.Alias.SelectField => s"_${selectField.fieldName}"
        flatName(relVal, s"$newSuffix$suffix")
      case _ => s"${member.getName}$suffix"

  // returns the name if the value is named, or the flat representation using
  // suffixes to differential between partial field selection or index application
  def flatName(using MemberGetSet): String = flatName(dfVal, "")

  // the partial name is set to be either a specialized dimension name (length/width)
  // or just the name of reference named member with a `_part` suffix
  private def partName(anonMember: DFVal, namedMember: DFVal)(using MemberGetSet): String =
    import DFRef.TypeRef
    val specialStrOpt =
      // only if the anonymous member has a constant value we do further checks
      if (anonMember.isConst)
        val originRefs = getSet.designDB.memberTable(anonMember)
        originRefs.view.flatMap {
          // found the member is referenced as a type
          case r: TypeRef =>
            // looking for what kind of type reference it is
            r.originMember.asInstanceOf[DFVal].dfType match
              case DFVector(_, (cellDimRef: TypeRef) :: _) if cellDimRef == r    => Some("length")
              case DFBits(widthRef: TypeRef) if widthRef == r                    => Some("width")
              case DFDecimal(widthParamRef = widthRef: TypeRef) if widthRef == r => Some("width")
              case _                                                             => None
          case _ => None
        }.headOption
      else None
    specialStrOpt match
      // upper casing to make the newly formed parameter name more visible
      case Some(specialStr) => s"${namedMember.flatName}_${specialStr}".toUpperCase()
      case _                => s"${namedMember.flatName}_part"
  end partName

  @tailrec private def suggestName(
      member: DFVal,
      prevMember: Option[DFVal] = None
  )(using MemberGetSet): Option[String] =
    val origins = member.originMembers

    val refOwner: Option[DFMember] =
      // search type referencing as origin first
      val first =
        if (member.isConst) origins.collectFirst {
          case dfVal: DFVal if !dfVal.isAnonymous && dfVal.dfType.getRefs.exists(_.get == member) =>
            dfVal
        }
        else None
      // search consuming references second
      val second = first.orElse {
        origins.filter {
          case _: (DFVal.Alias.Partial | DFConditional.Block) => false
          case _                                              => true
        }.headOption
      }
      // search aliasing references, as long as we don't go back to previous member
      // (aliasing can be used for both producing and consuming)
      second.orElse {
        origins.collectFirst {
          case m if prevMember.isEmpty || prevMember.get != m => m
        }
      }
    end refOwner
    refOwner match
      // name from assignment destination
      case Some(DFNet.Assignment(toVal = toVal)) => Some(partName(member, toVal))
      // name from connection destination
      case Some(DFNet.Connection(toVal = toVal: DFVal)) => Some(partName(member, toVal))
      // name from a named value which was referenced by an alias
      case Some(value: DFVal) if !value.isAnonymous => Some(partName(member, value))
      // found an (anonymous) value -> checking suggestion for it
      case Some(value: DFVal) => suggestName(value, Some(value))
      // no named source found
      case _ => None
  end suggestName
  def suggestName(using MemberGetSet): Option[String] =
    dfVal match
      case pbns: DFVal.PortByNameSelect =>
        Some(
          s"${pbns.designInstRef.get.getRelativeName(pbns.getOwner)}_${pbns.portNamePath}"
            .replace('.', '_')
        )
      case _ => suggestName(dfVal)
  // true if this is a variable that is never assigned/connected to
  def isConstVAR(using MemberGetSet): Boolean =
    dfVal match
      case dcl @ DclVar() =>
        dcl.getAssignmentsTo.isEmpty && dcl.getConnectionTo.isEmpty
      case _ => false
  def isAllowedMultipleReferences(using MemberGetSet): Boolean = dfVal match
    case _ if !dfVal.isAnonymous    => true // allow named
    case _: DFVal.Const             => true // allow anonymous constants
    case _: DFVal.Alias.History     => true // history values get proper names a dedicated stage
    case _: DFVal.Alias.ApplyIdx    => true // allow anonymous index selection
    case _: DFVal.Alias.SelectField => true // allow anonymous field selection
    case _: DFVal.PortByNameSelect  => true // allow anonymous port by name selection
    case OpaqueActual(_)            => true // allow anonymous opaque actual selection
    case _                          => false
  // true if this is a partial assignment or connection destination
  def isPartialNetDest(using MemberGetSet): Boolean = dfVal match
    case dfVal: DFVal.Alias.Partial =>
      dfVal.originMembers.headOption match
        case Some(DFNet.Assignment(toVal = toVal)) if toVal == dfVal => true
        case Some(DFNet.Connection(toVal = toVal)) if toVal == dfVal => true
        case _                                                       => false
    case _ => false
end extension

extension (refTW: DFNet.Ref)
  def isViaRef(using MemberGetSet): Boolean =
    refTW.originMember match
      case net: DFNet if net.isViaConnection =>
        refTW.get.stripPortSel.getOwner.isSameOwnerDesignAs(net)
      case _ => false

extension (origVal: DFVal)
  private def collectRelMembersRecur(
      forceIncludeOrigVal: Boolean
  )(using MemberGetSet): List[DFVal] =
    if (origVal.isAnonymous && !origVal.isGlobal || forceIncludeOrigVal)
      origVal :: origVal.getRefs.map(_.get).view
        .flatMap {
          case dfVal: DFVal => dfVal.collectRelMembersRecur(false)
          case _            => Nil
        }.toList
    else Nil
  @targetName("collectRelMembersDFVal")
  def collectRelMembers(includeOrigVal: Boolean)(using MemberGetSet): List[DFVal] =
    origVal.collectRelMembersRecur(includeOrigVal).reverse
end extension

extension (net: DFNet)
  @targetName("collectRelMembersDFNet")
  def collectRelMembers(using MemberGetSet): List[DFVal] =
    net match
      case DFNet(lhsRef = DFRef(lhs: DFVal), rhsRef = DFRef(rhs: DFVal)) =>
        lhs.collectRelMembers(false) ++ rhs.collectRelMembers(false)
      case _ => Nil

extension (textOut: TextOut)
  @targetName("collectRelMembersTextOut")
  def collectRelMembers(using MemberGetSet): List[DFVal] =
    textOut.getRefs.view
      .collect { case DFRef(dfVal: DFVal) => dfVal }
      .flatMap(_.collectRelMembers(false)).toList

extension (member: DFMember)
  private def isPublicMember(using MemberGetSet): Boolean =
    member match
      case DclPort()            => true
      case _: DFVal.DesignParam => true
      case _: DomainBlock       => true
      case _                    => false
end extension

extension (members: List[DFMember])
  def filterPublicMembers(using MemberGetSet): List[DFMember] =
    def getPublicMembersDeps(member: DFMember): List[DFMember] =
      member :: member.getRefs.flatMap { r =>
        val publicMemberCandidate = r.get
        publicMemberCandidate match
          case _: DFMember.Empty                                      => Nil
          case dfVal: DFVal.CanBeGlobal if dfVal.isGlobal             => Nil
          case _ if publicMemberCandidate.isSameOwnerDesignAs(member) =>
            getPublicMembersDeps(publicMemberCandidate)
          case _ => Nil
      }
    members.view.filter(_.isPublicMember).flatMap {
      case m => getPublicMembersDeps(m).reverse
    }.toList.distinct
end extension

extension (member: DFMember)
  def consumesCycles(using MemberGetSet): Boolean =
    member match
      case loop: DFLoop.Block =>
        loop.isInRTDomain && !loop.isCombinational
      case wait: Wait              => wait.isInRTDomain
      case _: StepBlock            => true
      case _: Goto                 => true
      case cb: DFConditional.Block =>
        cb.members(MemberView.Folded).exists(_.consumesCycles)
      case _ => false
end extension

/** An extractor that transforms a DFType using a provided update function.
  *
  * This extractor works in two phases:
  *   1. It first applies the `preCheck` predicate to determine if the type should be transformed
  *      and to extract a helper object of type H.
  *   2. Then it recursively processes any nested types (for structs, vectors, opaques).
  *   3. Finally, it applies the `updateFunc` to either the original type or the recursively
  *      processed type, along with the helper object.
  *
  * @tparam H
  *   The type of the helper object produced by preCheck and consumed by updateFunc
  */
class ComposedDFTypeReplacement[H](
    preCheck: DFType => Option[H],
    updateFunc: PartialFunction[(DFType, H), DFType]
)(using
    MemberGetSet
):
  Extractor =>
  def unapply(dfType: DFType): Option[DFType] =
    val composed = dfType match
      case dt: DFStruct =>
        val updatedMap = ListMap.from(dt.fieldMap.view.collect { case (name, Extractor(dfType)) =>
          (name, dfType)
        })
        if (updatedMap.nonEmpty) Some(dt.copy(fieldMap = updatedMap))
        else None
      case dt: DFOpaque =>
        dt.actualType match
          case Extractor(dfType) => Some(dt.copy(actualType = dfType))
          case _                 => None
      case dt: DFVector =>
        dt.cellType match
          case Extractor(dfType) => Some(dt.copy(cellType = dfType))
          case _                 => None
      case _ => None
    // if the original type matches the precheck, the extractor will always return some updated value
    preCheck(dfType) match
      case Some(helper) => Some(updateFunc(composed.getOrElse(dfType), helper))
      case None         => composed
  end unapply
end ComposedDFTypeReplacement
