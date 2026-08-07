package dfhdl.compiler.stages

import dfhdl.compiler.analysis.*
import dfhdl.compiler.ir.*
import dfhdl.compiler.patching.*
import dfhdl.internals.*
import dfhdl.options.CompilerOptions
import DFVal.Func.Op as FuncOp
import dfhdl.compiler.stages.vhdl.VHDLDialect
import dfhdl.compiler.ir.DFConditional.DFMatchHeader
import dfhdl.compiler.stages.verilog.VerilogDialect
import scala.annotation.tailrec

// Names an anonymous relative value which is aliased.
// The aliasing is limited according to the criteria provided
private abstract class NamedAliases extends HierarchyStage:
  def dependencies: List[Stage] = Nil
  def nullifies: Set[Stage] =
    Set(DFHDLUniqueNames, DropLocalDcls, ExplicitNamedVars, DropUnreferencedAnons)
  def criteria(dfVal: DFVal)(using MemberGetSet, CompilerOptions): List[DFVal]

  // The conditional header a value has to be named in front of, if naming it where it was built
  // would leave the name inside a conditional expression branch in a concurrent scope. Such a
  // position is illegal (see `DB.condExprNamedValCheck`): the branch is not a block, so
  // `ExplicitNamedVars` would drive the name by connection, and once `ExplicitCondExprAssign`
  // wraps the expression into a `process(all)` that connection prints as an `assign` inside an
  // `always_comb` in Verilog, and in VHDL as a signal assignment the next statement reads a delta
  // cycle too early. `getTopConditionalHeader` climbs to the outermost conditional in the same
  // scope, the anchor `DropLocalDcls` also uses. A conditional chain never crosses a domain
  // boundary, so the walk stops there, which keeps it clear of the top's empty owner ref.
  private def hoistAnchorOf(dfVal: DFVal)(using MemberGetSet): Option[DFConditional.Header] =
    @tailrec def condExprBlockOf(member: DFMember): Option[DFConditional.Block] =
      member.getOwner match
        case cb: DFConditional.Block =>
          if (cb.getHeaderCB.dfType == DFUnit) condExprBlockOf(cb) else Some(cb)
        case _: DFDomainOwner => None
        case owner            => condExprBlockOf(owner)
    if (dfVal.isInEDDomain && !dfVal.isInProcess)
      condExprBlockOf(dfVal).map(_.getTopConditionalHeader)
    else None

  // Everything that has to travel with the hoisted value: its anonymous dependency cone, and for
  // any conditional expression in that cone the whole construct, since a header cannot leave the
  // branch without the blocks it owns. Members already outside the branch stay where they are.
  // The cone is ordered dependencies-first, which is the order the moved members must keep.
  private def hoistMembers(named: DFVal)(using MemberGetSet): List[DFMember] =
    named.collectRelMembers(includeOrigVal = true).flatMap {
      case relVal if hoistAnchorOf(relVal).isEmpty => Nil
      case ch: DFConditional.Header                =>
        ch :: ch.getCBList.flatMap(cb => cb :: cb.members(MemberView.Flattened))
      case relVal => List(relVal)
    }
  // One naming pass. Returns an empty list once nothing anonymous meets the criteria any more,
  // which is what terminates the loop in `transformSubDB`.
  private def collectPatches(db: DB)(using MemberGetSet, CompilerOptions): List[(DFMember, Patch)] =
    val patches = db.members.view
      // just values
      .collect { case dfVal: DFVal if dfVal.isAnonymous => dfVal }
      // filter out partial net destinations
      .filterNot(_.isPartialNetDest)
      // get all that meet the criteria
      .flatMap(criteria)
      // filter out the trivial cases (no need to name)
      .filterNot(_.isAllowedMultipleReferences)
      // tuple with the suggested name
      .map(m => (m, m.suggestName.getOrElse("anon")))
      // group dfhdl-equivalent values, as long as they are in the same scope.
      // conditional headers are excluded from grouping because their =~ comparison
      // does not account for block contents (conditions/branches), so structurally
      // different conditionals could be incorrectly merged.
      .groupByCompare(
        (l, r) =>
          !l._1.isInstanceOf[DFConditional.Header] &&
            l._1 =~ r._1 && l._1.isInsideOwner(r._1.getOwner),
        _._2.hashCode()
      )
      // split to list of aliases and list of suggested names for each group
      .map(_.unzip)
      // for each group use just the head to create the named member, along with the members that
      // have to travel with it when its position cannot hold a name
      .collect { case (firstAlias :: restOfAliases, suggestedName :: _) =>
        // we force set the underlying original name before it was anonymized
        val namedMember = firstAlias.setName(suggestedName)
        val moved = hoistAnchorOf(firstAlias).map(anchor => (anchor, hoistMembers(firstAlias)))
        (firstAlias, namedMember, restOfAliases, moved)
      }.toList
    // Two groups can both need to relocate the same sub-tree, because the cone of an outer value
    // reaches a conditional expression that is a naming group of its own (`(if (d) p else q) + 1`
    // names both). Moving it twice duplicates it in the member list, so only the innermost group
    // acts in this pass: a group whose own value another group would carry is dropped entirely,
    // left anonymous, and picked up by the next pass, once the value it reads sits outside the
    // branch and no longer moves with it.
    val carriedByOthers = patches.view.flatMap { case (firstAlias, _, _, moved) =>
      moved.view.flatMap(_._2).filterNot(_ eq firstAlias)
    }.toSet
    patches.filterNot((firstAlias, _, _, _) => carriedByOthers.contains(firstAlias))
      .flatMap { (firstAlias, namedMember, restOfAliases, moved) =>
        // the first alias is named in place, unless that place cannot hold a name, in which case
        // it is named and relocated together. `ChangeRefAndRemove` is what makes the two one
        // patch: it drops the original from its illegal position and redirects its readers, while
        // the move inserts the *named* instance before the conditional. `FullReplacement` could
        // not be used here, because `Patch.Move` emits a `Remove` per moved member and
        // `Replace` + `Remove` on one member is not a mergeable combination.
        val firstPatches = moved match
          case Some((anchor, movedMembers)) =>
            val relocated = movedMembers.map {
              case m if m eq firstAlias => namedMember
              case m                    => m
            }
            List(
              firstAlias -> Patch.Replace(namedMember, Patch.Replace.Config.ChangeRefAndRemove),
              anchor -> Patch.Move(relocated, firstAlias.getOwner, Patch.Move.Config.Before)
            )
          case None =>
            List(firstAlias -> Patch.Replace(namedMember, Patch.Replace.Config.FullReplacement))
        // the rest of the aliases (if there are any) are just a reference change
        firstPatches ::: restOfAliases.map(
          _ -> Patch.Replace(
            namedMember,
            Patch.Replace.Config.ChangeRefOnly
          )
        )
      }
  end collectPatches

  def transformSubDB(rootDB: DB)(using MemberGetSet, CompilerOptions, RefGen): DB =
    // a pass that only handled the innermost of two overlapping groups leaves the outer one
    // anonymous, so it is still a criteria match and the next pass picks it up
    @tailrec def recur(db: DB)(using MemberGetSet): DB =
      val patches = collectPatches(db)
      if (patches.isEmpty) db
      else
        val patchedDB = db.patch(patches)
        recur(patchedDB)(using patchedDB.getSet)
    recur(subDB)
  end transformSubDB
end NamedAliases

// For verilog bit selection from unnamed values is limited.
// This compilation stage names the intermediate values. A future stage (UniqueNames) is responsible for
// making sure the names will be unique.
case object NamedVerilogSelection extends NamedAliases:
  override def runCondition(using co: CompilerOptions): Boolean = co.backend.isVerilog
  extension (dfVal: DFVal)(using MemberGetSet)
    def hasVerilogName: Boolean =
      dfVal match
        case dfVal if !dfVal.isAnonymous => true
        case alias: DFVal.Alias.AsIs     =>
          val relVal = alias.relValRef.get
          val transparentConversion = (alias.dfType, relVal.dfType) match
            case (DFUInt(toWidthRef), DFBits(fromWidthRef)) => toWidthRef.isSimilarTo(fromWidthRef)
            case (DFBits(toWidthRef), DFUInt(fromWidthRef)) => toWidthRef.isSimilarTo(fromWidthRef)
            case (DFBit, DFBool)                            => true
            case (DFBool, DFBit)                            => true
            case _                                          => false
          if (transparentConversion) relVal.hasVerilogName
          else false
        case _ => false
  end extension
  def criteria(dfVal: DFVal)(using getSet: MemberGetSet, co: CompilerOptions): List[DFVal] =
    def isBasicVerilog = co.backend match
      case be: dfhdl.backends.verilog =>
        be.dialect match
          case VerilogDialect.v95 | VerilogDialect.v2001 => true
          case _                                         => false
      case _ => false
    dfVal match
      case alias: DFVal.Alias if alias.relValRef.get.hasVerilogName => Nil
      case alias: DFVal.Alias.ApplyRange
          if alias.compareWidths(alias.relValRef.get)(_ != _).getOrElse(true) =>
        List(alias.relValRef.get)
      case alias @ DFVal.Alias.AsIs(
            dfType = _: (DFDecimal | DFBits),
            relValRef = DFRef(relVal @ (DFBits.Val(_) | DFDecimal.Val(_)))
          )
          if alias.compareWidths(relVal)(_ < _).getOrElse(true) =>
        if (relVal.dfType == DFInt32)
          Nil // conversion from DFInt32 is not a bit selection, so no need to break the expression
        else
          // systemverilog truncation is does not use partial selection, but old verilog does,
          // so we need to name the value in old verilog
          if (isBasicVerilog) List(alias.relValRef.get)
          else Nil
      // A width-WIDENING resize over an anonymous expression: basic Verilog's EXTEND macros
      // embed the operand in a replication/concatenation (self-determined width, and the
      // EXTEND_S variants bit-select the operand, which must be an indexable primary), while
      // the SystemVerilog W'() cast evaluates its operand AT THE CAST WIDTH. An anonymous
      // func operand would therefore evaluate at the wrong width (or print illegally), so it
      // is named, pinning the evaluation at its declared width; in basic Verilog any other
      // anonymous non-primary operand is named for the EXTEND_S syntax constraint. The
      // exception: a resize over an anonymous sign-conversion alias prints FUSED as a single
      // zero-extension of the conversion's own operand (see `csDFValAliasAsIs`), so it needs
      // no name.
      case alias @ DFVal.Alias.AsIs(
            dfType = _: (DFDecimal | DFBits),
            relValRef = DFRef(relVal @ (DFBits.Val(_) | DFDecimal.Val(_)))
          )
          if relVal.dfType != DFInt32 && alias.compareWidths(relVal)(_ > _).getOrElse(false) =>
        relVal match
          case signConv: DFVal.Alias.AsIs
              if signConv.isAnonymous &&
                ((signConv.dfType, signConv.relValRef.get.dfType) match
                  case (DFSInt(_), DFUInt(_)) => true
                  case _ => false) =>
            Nil // fused sign-conversion emission
          case func: DFVal.Func    => List(func)
          case _ if isBasicVerilog => List(relVal)
          case _                   => Nil
      // to/from vector conversion is used with selection
      case DFVal.Alias.AsIs(dfType = DFVector(_, _), relValRef = DFRef(relVal @ DFBits.Val(_))) =>
        // in basic verilog this casting is only kept for initial values and later ignored by the backend
        // preventing basic verilog compilation from naming the casted value
        if (isBasicVerilog) Nil
        else List(relVal)
      case DFVal.Alias.AsIs(dfType = DFBits(_), relValRef = DFRef(relVal @ DFVector.Val(_))) =>
        List(relVal)
      case alias: DFVal.Alias.ApplyIdx =>
        List(alias.relValRef.get)
      case func: DFVal.Func =>
        func.getReadDeps.headOption match
          case Some(dfVal: DFVal) => criteria(dfVal)
          case _                  => Nil
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
    end match
  end criteria
end NamedVerilogSelection

// For vhdl patten matching of a selection is limited.
case object NamedVHDLSelection extends NamedAliases:
  override def runCondition(using co: CompilerOptions): Boolean =
    co.backend match
      case be: dfhdl.backends.vhdl =>
        be.dialect match
          case VHDLDialect.v93 => true
          case _               => false
      case _ => false
  def criteria(dfVal: DFVal)(using MemberGetSet, CompilerOptions): List[DFVal] =
    dfVal.getReadDeps.headOption match
      case Some(_: DFConditional.DFMatchHeader) => List(dfVal)
      case _                                    => Nil
end NamedVHDLSelection

extension [T: HasDB](t: T)
  def verilogNamedSelection(using CompilerOptions): DB =
    StageRunner.run(NamedVerilogSelection)(t.db)

// Creating a previous values of a value requires that value to be names to avoid random anonymous names in the
// the backend
case object NamedPrev extends NamedAliases:
  def criteria(dfVal: DFVal)(using MemberGetSet, CompilerOptions): List[DFVal] = dfVal match
    case alias: DFVal.Alias.History if alias.relValRef.get.isAnonymous =>
      List(alias.relValRef.get)
    case _ => Nil

extension [T: HasDB](t: T)
  def namedPrev(using CompilerOptions): DB = StageRunner.run(NamedPrev)(t.db)

// Names an anonymous value which is referenced more than once
case object NamedAnonMultiref extends NamedAliases, NoCheckStage:
  def criteria(dfVal: DFVal)(using MemberGetSet, CompilerOptions): List[DFVal] = dfVal match
    case dfVal if !dfVal.isAnonymous => Nil
    case dfVal                       =>
      // referenced more than once (excluding else/case blocks referencing their headers & type refs)
      val refs = getSet.designDB.memberTable.getOrElse(dfVal, Set()).view.flatMap {
        case _: DFRef.TypeRef                                                => None
        case r: DFRef.TwoWayAny if !r.get.isInstanceOf[DFConditional.Header] =>
          getSet.designDB.originRefTable.get(r) match
            case Some(_: DFDesignInst) => None // skipping design param references
            case Some(_)               => Some(r)
            case other                 => None
        case _ => None
      }
      if (refs.size > 1) List(dfVal)
      else Nil
end NamedAnonMultiref

//Names anonymous conditional expressions, as long as they are not the result of an enclosing
//construct that carries them through its own lowering, and as long as they are not directly
//assigned to a declaration or connected to an output port
case object NamedAnonCondExpr extends NamedAliases:
  override def dependencies: List[Stage] = List()
  def criteria(dfVal: DFVal)(using MemberGetSet, CompilerOptions): List[DFVal] = dfVal match
    case dfVal: DFConditional.Header if dfVal.isAnonymous && dfVal.dfType != DFUnit =>
      val nameIt =
        dfVal.getReadDeps.collectFirst {
          // directly assigned to a declaration (variable or output port)
          case DFNet.Assignment(toVal = _: DFVal.Dcl) => false
          // directly connected to an output port
          case DFNet.Connection(toVal = DclOut()) => false
          // referenced by an ident that trails a scope (a conditional-expression branch, or a
          // fall-through step block), which means the conditional is that scope's result and the
          // enclosing construct lowers it along with itself.
          //
          // A method's return wiring is an ident too, but it trails the method body rather than a
          // scope, so nothing downstream would lower the conditional under it and the backend
          // would be left printing a conditional expression it has no form for. Such a
          // conditional is named here, and `ExplicitNamedVars` turns the name into a variable
          // that each branch assigns and the return ident then reads.
          case ident @ Ident(_) if !ident.getOwner.isInstanceOf[DFDesignBlock] => false
        }.getOrElse(true)
      if (nameIt) List(dfVal)
      else Nil
    case dfVal => Nil
end NamedAnonCondExpr

extension [T: HasDB](t: T)
  def namedAnonMultiref(using CompilerOptions): DB =
    StageRunner.run(NamedAnonMultiref)(t.db)

extension [T: HasDB](t: T)
  def namedAnonCondExpr(using CompilerOptions): DB =
    StageRunner.run(NamedAnonCondExpr)(t.db)
