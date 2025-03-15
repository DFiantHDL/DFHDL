package dfhdl.compiler.stages

import dfhdl.compiler.analysis.*
import dfhdl.compiler.ir.*
import dfhdl.compiler.patching.*
import dfhdl.internals.*
import dfhdl.options.CompilerOptions
import DFVal.Func.Op as FuncOp
import scala.reflect.{ClassTag, classTag}

// Names an anonymous relative value which is aliased.
// The aliasing is limited according to the criteria provided
private abstract class NamedAliases extends Stage:
  def dependencies: List[Stage] = Nil
  def nullifies: Set[Stage] =
    Set(DFHDLUniqueNames, DropLocalDcls, ExplicitNamedVars, DropUnreferencedAnons)
  def criteria(dfVal: DFVal)(using MemberGetSet): List[DFVal]
  def transform(designDB: DB)(using MemberGetSet, CompilerOptions): DB =

    val patchList: List[(DFMember, Patch)] =
      designDB.members.view
        // just values
        .collect { case dfVal: DFVal if dfVal.isAnonymous => dfVal }
        // get all that meet the criteria
        .flatMap(criteria)
        // filter out the trivial cases (no need to name)
        .filterNot(_.isAllowedMultipleReferences)
        // tuple with the suggested name
        .map(m => (m, m.suggestName.getOrElse("anon")))
        // group dfhdl-equivalent values, as long as they are in the same scope
        .groupByCompare(
          (l, r) => l._1 =~ r._1 && l._1.isInsideOwner(r._1.getOwner),
          _._2.hashCode()
        )
        // split to list of aliases and list of suggested names for each group
        .map(_.unzip)
        // for each group use just the head to create the named member and patch
        // all members to reference that member
        .flatMap {
          case (firstAlias :: restOfAliases, suggestedName :: _) =>
            // we force set the underlying original name before it was anonymized
            val namedMember = firstAlias.setName(suggestedName)
            // first alias gets a full replacement
            val firstPatch =
              firstAlias -> Patch.Replace(namedMember, Patch.Replace.Config.FullReplacement)
            // the rest of the aliases (if there are any) are just a reference change
            firstPatch :: restOfAliases.map(
              _ -> Patch.Replace(
                namedMember,
                Patch.Replace.Config.ChangeRefOnly
              )
            )
          case _ => Nil
        }.toList
    designDB.patch(patchList)
  end transform
end NamedAliases

// For verilog bit selection from unnamed values is limited.
// This compilation stage names the intermediate values. A future stage (UniqueNames) is responsible for
// making sure the names will be unique.
case object NamedVerilogSelection extends NamedAliases:
  override def runCondition(using co: CompilerOptions): Boolean = co.backend.isVerilog
  private val carryOps = Set(FuncOp.`*`, FuncOp.+, FuncOp.-)
  extension (dfVal: DFVal)(using MemberGetSet)
    def hasVerilogName: Boolean =
      dfVal match
        case dfVal if !dfVal.isAnonymous => true
        case alias: DFVal.Alias.AsIs =>
          val relVal = alias.relValRef.get
          val transparentConversion = (alias.dfType, relVal.dfType) match
            case (DFUInt(Int(toWidth)), DFBits(Int(fromWidth))) => toWidth == fromWidth
            case (DFBits(Int(toWidth)), DFUInt(Int(fromWidth))) => toWidth == fromWidth
            case (DFBit, DFBool)                                => true
            case (DFBool, DFBit)                                => true
            case _                                              => false
          if (transparentConversion) relVal.hasVerilogName
          else false
        case _ => false
  end extension
  def criteria(dfVal: DFVal)(using MemberGetSet): List[DFVal] = dfVal match
    case alias: DFVal.Alias if alias.relValRef.get.hasVerilogName => Nil
    case alias: DFVal.Alias.ApplyRange if alias.width != alias.relValRef.get.width =>
      List(alias.relValRef.get)
    case alias: DFVal.Alias.AsIs if alias.width < alias.relValRef.get.width =>
      if (alias.relValRef.get.dfType == DFInt32)
        Nil // conversion from DFInt32 is not a bit selection, so no need to break the expression
      else List(alias.relValRef.get)
    // to/from vector conversion is used with selection
    case DFVal.Alias.AsIs(dfType = DFVector(_, _), relValRef = DFRef(relVal @ DFBits.Val(_))) =>
      List(relVal)
    case DFVal.Alias.AsIs(dfType = DFBits(_), relValRef = DFRef(relVal @ DFVector.Val(_))) =>
      List(relVal)
    case alias: DFVal.Alias.ApplyIdx =>
      List(alias.relValRef.get)
    case func @ DFVal.Func(op = op, args = DFRef(lhs) :: _ :: Nil)
        if !lhs.hasVerilogName && carryOps.contains(op) && func.width > lhs.width =>
      List(lhs)
    // anonymous conditional expressions
    case ch: DFConditional.Header if ch.isAnonymous && ch.dfType != DFUnit =>
      ch.getReadDeps.head match
        // if the conditional is referred from a net, it is not a selection to be named
        case net: DFNet => Nil
        // if the conditional is referred from an ident, it is not a selection to be named
        case Ident(_) => Nil
        // otherwise, it is a selection to be named
        case _ => List(ch)
    case _ => Nil
end NamedVerilogSelection

extension [T: HasDB](t: T)
  def verilogNamedSelection(using CompilerOptions): DB =
    StageRunner.run(NamedVerilogSelection)(t.db)

// Creating a previous values of a value requires that value to be names to avoid random anonymous names in the
// the backend
case object NamedPrev extends NamedAliases:
  def criteria(dfVal: DFVal)(using MemberGetSet): List[DFVal] = dfVal match
    case alias: DFVal.Alias.History if alias.relValRef.get.isAnonymous =>
      List(alias.relValRef.get)
    case _ => Nil

extension [T: HasDB](t: T)
  def namedPrev(using CompilerOptions): DB = StageRunner.run(NamedPrev)(t.db)

// Names an anonymous value which is referenced more than once
case object NamedAnonMultiref extends NamedAliases, NoCheckStage:
  private val cbTags: Set[ClassTag[?]] =
    Set(classTag[DFConditional.DFMatchHeader], classTag[DFConditional.DFIfHeader])
  def criteria(dfVal: DFVal)(using MemberGetSet): List[DFVal] = dfVal match
    case dfVal if !dfVal.isAnonymous => Nil
    case dfVal                       =>
      // referenced more than once (excluding else/case blocks referencing their headers & type refs)
      val refs = getSet.designDB.memberTable.getOrElse(dfVal, Set()).view.flatMap {
        case _: DFRef.TypeRef                                  => None
        case r: DFRef.TwoWayAny if !cbTags.contains(r.refType) => Some(r)
        case _                                                 => None
      }
      if (refs.size > 1) List(dfVal)
      else Nil

//Names anonymous conditional expressions, as long as they are not referenced by an ident which indicates that
//they are themselves inside another conditional expression
case object NamedAnonCondExpr extends NamedAliases:
  override def dependencies: List[Stage] = List(ExplicitCondExprAssign)
  def criteria(dfVal: DFVal)(using MemberGetSet): List[DFVal] = dfVal match
    case dfVal: DFConditional.Header if dfVal.isAnonymous && dfVal.dfType != DFUnit =>
      val isReferencedByIdent =
        dfVal.getReadDeps.collectFirst { case Ident(_) => true }.getOrElse(false)
      if (isReferencedByIdent) Nil
      else List(dfVal)
    case dfVal => Nil

extension [T: HasDB](t: T)
  def namedAnonMultiref(using CompilerOptions): DB =
    StageRunner.run(NamedAnonMultiref)(t.db)
