package dfhdl.compiler.stages

import dfhdl.compiler.analysis.*
import dfhdl.compiler.ir.*
import dfhdl.compiler.patching.*
import dfhdl.internals.*
import dfhdl.options.CompilerOptions
import scala.collection.immutable.ListMap
import scala.collection.mutable
import dfhdl.compiler.stages.vhdl.VHDLDialect
import dfhdl.core.{DFTypeAny, asFE, refTW}
import dfhdl.compiler.ir.DFVal.PortByNameSelect

/** This stage globalizes design parameters that set port vector lengths. This is needed only for
  * vhdl.v93 that does not support arrays with unconstrained ranges.
  */
case object GlobalizePortVectorParams extends Stage:
  def dependencies: List[Stage] = List(UniqueDesigns)
  def nullifies: Set[Stage] = Set(DropUnreferencedAnons, DFHDLUniqueNames)
  override def runCondition(using co: CompilerOptions): Boolean =
    co.backend match
      case be: dfhdl.backends.vhdl =>
        be.dialect match
          case VHDLDialect.v93 => true
          case _               => false
      case _ => false
  def transform(designDB: DB)(using MemberGetSet, CompilerOptions): DB =
    given RefGen = RefGen.fromGetSet
    // First, reconstruct members for duplicate designs (which have no members in the DB).
    // This is needed so the parameter analysis below can find design params for all designs.
    val dupDesignDB =
      val dupRefTable = mutable.Map.empty[DFRefAny, DFMember]
      val dupDesignMembersMap = mutable.Map.empty[DFDesignBlock, List[DFMember]]
      // Duplicate all members of a design, recursively handling nested designs.
      def duplicateDesignMembers(
          orig: DFDesignBlock,
          dup: DFDesignBlock
      ): Unit =
        val origMembers = designDB.designMemberTable(orig)
        val origToDupMemberMap = mutable.Map.empty[DFMember, DFMember]
        origToDupMemberMap += orig -> dup
        def getReplacement(member: DFMember): DFMember =
          origToDupMemberMap.getOrElse(member, member)
        val dupMembers = origMembers.map { origMember =>
          val dupMember0 = origMember.copyWithNewRefs
          // Tag nested design blocks as duplicates
          val dupMember = dupMember0 match
            case dsn: DFDesignBlock =>
              dsn.setTags(_.tag(DuplicateTag)).asInstanceOf[dupMember0.type]
            case _ => dupMember0
          origToDupMemberMap += origMember -> dupMember
          dupRefTable += dupMember.ownerRef -> getReplacement(origMember.getOwner)
          origMember.getRefs.lazyZip(dupMember.getRefs).foreach { (origRef, dupRef) =>
            dupRefTable += dupRef -> getReplacement(origRef.get)
          }
          dupMember
        }
        dupDesignMembersMap += dup -> dupMembers
        // Recursively duplicate nested designs that were also removed
        origMembers.lazyZip(dupMembers).foreach {
          case (origNested: DFDesignBlock, dupNested: DFDesignBlock)
              if !designDB.designMemberTable.contains(dupNested) =>
            duplicateDesignMembers(origNested, dupNested)
          case _ =>
        }
      end duplicateDesignMembers
      designDB.dupDesignToOrigMap.groupBy(_._2).foreach { (orig, dupMap) =>
        dupMap.keys.foreach { dup => duplicateDesignMembers(orig, dup) }
      }
      def populateWithDupMembers(members: List[DFMember]): List[DFMember] =
        members.flatMap {
          case design: DFDesignBlock =>
            design :: populateWithDupMembers(
              dupDesignMembersMap.getOrElse(design, designDB.designMemberTable(design))
            )
          case member => Some(member)
        }
      designDB.copy(
        members = designDB.membersGlobals ++ populateWithDupMembers(List(designDB.top)),
        refTable = designDB.refTable ++ dupRefTable
      )
    end dupDesignDB
    // Now run the analysis on the reconstructed DB with all design members present.
    locally {
      given MemberGetSet = dupDesignDB.getSet
      // to collect unique design parameters while maintaining order for consistent compilation and dependency
      val designParams = mutable.LinkedHashSet.empty[DFVal]
      // check ref
      def checkRef(ref: DFRef.TwoWayAny): Int =
        ref.get match
          case dfVal: DFVal =>
            if (dfVal.isGlobal) 0
            else
              val ret = dfVal.getRefs.map(checkRef).sum
              if (dfVal.isAnonymous) ret
              else
                designParams += dfVal
                ret + 1
          case _ => 0
      // check int param ref
      def checkIntParamRef(intParamRef: IntParamRef): Int =
        intParamRef.getRef.map(checkRef).getOrElse(0)

      // checking vector types
      def checkVector(dfType: DFType): Int = dfType match
        case dt: DFVector =>
          // checking vector dimensions for parameters we need to globalize
          val dimParamCnt = dt.cellDimParamRefs.map(checkIntParamRef).sum
          // checking vector cell type for composed dependency on parameters we need to globalize
          val cellTypeParamCnt = dt.cellType match
            case DFBits(widthParamRef) => checkIntParamRef(widthParamRef)
            case DFUInt(widthParamRef) => checkIntParamRef(widthParamRef)
            case DFSInt(widthParamRef) => checkIntParamRef(widthParamRef)
            case dt: DFVector          => checkVector(dt.cellType)
            case _                     => 0
          dimParamCnt + cellTypeParamCnt
        case _ => 0
      val vecTypeReplaceMap = mutable.Map.empty[DFVector, DFVector]
      // vector type extractor through reference and port by name select
      object VectorNetNodeType:
        def unapply(ref: DFRefAny): Option[DFVector] = ref.get match
          case PortByNameSelect.Of(DFVector.Val(dfType)) => Some(dfType)
          case DFVector.Val(dfType)                      => Some(dfType)
          case _                                         => None
      dupDesignDB.members.foreach {
        // checking all ports
        case dcl @ DclPort() => checkVector(dcl.dfType)
        // checking all port by name selects that change their type
        case pbns @ PortByNameSelect.Of(DFVector.Val(dclType)) if pbns.dfType != dclType =>
          vecTypeReplaceMap += pbns.dfType.asInstanceOf[DFVector] -> dclType
        // checking all assignments/connections between vectors that are considered to be similar types,
        // but are not exactly the same (e.g., two vectors types referencing a `(LEN + 1)` length value)
        case net @ DFNet(lhsRef = VectorNetNodeType(lhsType), rhsRef = VectorNetNodeType(rhsType))
            if !(lhsType == rhsType) && lhsType.isSimilarTo(rhsType) =>
          val lhsCnt = checkVector(lhsType)
          val rhsCnt = checkVector(rhsType)
          // when a PBNS connects to a non-PBNS, always unify to the child design's port type
          // so that after globalization both sides share the same instance-specific globals
          val lhsIsPbns = net.lhsRef.get.isInstanceOf[DFVal.PortByNameSelect]
          val rhsIsPbns = net.rhsRef.get.isInstanceOf[DFVal.PortByNameSelect]
          if (lhsIsPbns && !rhsIsPbns) vecTypeReplaceMap += rhsType -> lhsType
          else if (rhsIsPbns && !lhsIsPbns) vecTypeReplaceMap += lhsType -> rhsType
          else if (lhsCnt < rhsCnt) vecTypeReplaceMap += lhsType -> rhsType
          else if (lhsCnt > rhsCnt) vecTypeReplaceMap += rhsType -> lhsType
        case _ =>
      }
      // Split vecTypeReplace into PBNS patches and non-PBNS patches.
      // PBNS replacements introduce TypeRefs from port declarations (shared TypeRefs).
      // If applied in the same patch batch as port replacements (which purge those same TypeRefs),
      // the TypeRefs get removed before the PBNS can reference them.
      // Apply PBNS replacements first so the shared TypeRefs have a higher repeat count.
      val (vecTypePbnsPatches, vecTypeOtherPatches) = dupDesignDB.members.collect {
        case dfVal @ DFVector.Val(dfType) if vecTypeReplaceMap.contains(dfType) =>
          dfVal -> Patch.Replace(
            dfVal.updateDFType(vecTypeReplaceMap(dfType)),
            Patch.Replace.Config.FullReplacement
          )
      }.partition((m, _) => m.isInstanceOf[DFVal.PortByNameSelect])
      def movedMembers(namedParam: DFVal): List[DFVal] =
        namedParam match
          // for DesignParams, also collect anonymous deps of the actual param value
          // (from paramMap), since collectRelMembers only follows getRefs which
          // does not include the paramMap value reference
          case param: DFVal.DesignParam =>
            param.appliedOrDefaultVal.collectRelMembers(false).filterNot(_.isGlobal) ++ List(param)
          case _ =>
            namedParam.collectRelMembers(true).filterNot(_.isGlobal)

      val addedGlobals = designParams.view.flatMap(movedMembers).toList.distinct
      val dupDesignSet = designParams.view.map(_.getOwnerDesign).toSet
      // origin -> duplicates design map
      val dupDesignMap = dupDesignSet.groupBy(_.dclName).values.map { grp =>
        val (dups, orig) = grp.partition(_.isDuplicate)
        assert(orig.size == 1)
        orig.head -> dups.toList
      }.toMap

      // patches to change the duplicated design declaration names to a unique identifier according
      // to their instance names.
      // Only remove globalized params from paramMap; keep unglobalized ones intact.
      val globalizedParamNamesPerDesign: Map[DFDesignBlock, Set[String]] =
        designParams.view.collect { case dp: DFVal.DesignParam => dp }
          .groupBy(_.getOwnerDesign)
          .view.mapValues(_.map(_.getName).toSet).toMap
      def prunedParamMap(design: DFDesignBlock): ListMap[String, DFDesignBlock.ParamRef] =
        val toRemove = globalizedParamNamesPerDesign.getOrElse(design, Set.empty)
        if (toRemove.isEmpty) design.paramMap
        else design.paramMap.filterNot((name, _) => toRemove.contains(name))
      val designPatches = dupDesignMap.view.flatMap {
        case (orig, dups) if dups.length >= 1 =>
          (orig :: dups).map { design =>
            val updatedDclMeta =
              design.dclMeta.setName(
                s"${design.dclName}_${design.getFullName.replaceAll("\\.", "_")}"
              )
            val updatedDesign = design.removeTagOf[DuplicateTag].copy(
              dclMeta = updatedDclMeta,
              paramMap = prunedParamMap(design)
            )
            design -> Patch.Replace(updatedDesign, Patch.Replace.Config.FullReplacement)
          }
        case (orig, _) =>
          Some(
            orig ->
              Patch.Replace(
                orig.copy(paramMap = prunedParamMap(orig)),
                Patch.Replace.Config.FullReplacement
              )
          )
      }.toList
      val paramReplacementMap = mutable.Map.empty[DFVal, DFVal]
      def getUpdatedParamValue(param: DFVal.DesignParam): DFVal =
        var dfVal = param.appliedOrDefaultVal
        while (paramReplacementMap.contains(dfVal)) dfVal = paramReplacementMap(dfVal)
        dfVal
      // add global parameters before the design top
      val dsn = new MetaDesign(dupDesignDB.top, Patch.Add.Config.Before):
        // patches to replace with properly named parameter or just move the anonymous members
        val replacePatches = addedGlobals.map {
          // design parameters are transformed into global as-is named aliases
          case param: DFVal.DesignParam =>
            val updatedMeta = param.meta.setName(param.getFullName.replaceAll("\\.", "_"))
            val updatedParamVal = getUpdatedParamValue(param)
            val globalParam = dfhdl.core.DFVal.Alias.AsIs.forced(
              param.dfType,
              updatedParamVal
            )(using dfc.setMeta(updatedMeta))
            paramReplacementMap += param -> globalParam
            param -> Patch.Replace(globalParam, Patch.Replace.Config.ChangeRefAndRemove)
          case m: DFVal if !m.isAnonymous =>
            val globalParam = m.setName(m.getFullName.replaceAll("\\.", "_"))
            plantMember(globalParam)
            paramReplacementMap += m -> globalParam
            m -> Patch.Replace(globalParam, Patch.Replace.Config.ChangeRefAndRemove)
          case m =>
            plantMember(m)
            m -> Patch.Remove(isMoved = true)
        }
      // TODO: when combined to a single patch, there is a bug that prevents some members to get a global ownership
      // Apply PBNS type patches first (they introduce shared TypeRefs), then other type patches
      // (which may purge those same TypeRefs from the originals).
      dupDesignDB
        .patch(dsn.patch :: dsn.replacePatches ++ vecTypePbnsPatches)
        .patch(vecTypeOtherPatches)
        .patch(designPatches)
    }
  end transform
end GlobalizePortVectorParams

extension [T: HasDB](t: T)
  def globalizePortVectorParams(using CompilerOptions): DB =
    StageRunner.run(GlobalizePortVectorParams)(t.db)
