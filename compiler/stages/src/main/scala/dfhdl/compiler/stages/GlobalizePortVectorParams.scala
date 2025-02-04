package dfhdl.compiler.stages

import dfhdl.compiler.analysis.*
import dfhdl.compiler.ir.*
import dfhdl.compiler.patching.*
import dfhdl.internals.*
import dfhdl.options.CompilerOptions
import scala.collection.immutable.ListMap
import scala.collection.mutable
import dfhdl.compiler.stages.vhdl.VHDLDialect
import dfhdl.core.{DFTypeAny, asFE}
import dfhdl.compiler.ir.DFVal.Alias.DesignParamTag
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
    // to collect unique design parameters while maintaining order for consistent compilation and dependency
    val designParams = mutable.LinkedHashSet.empty[DFVal]
    // check ref
    def checkRef(ref: DFRef.TwoWayAny): Int =
      val dfVal = ref.get.asInstanceOf[DFVal]
      if (dfVal.isGlobal) 0
      else
        val ret = dfVal.getRefs.map(checkRef).sum
        if (dfVal.isAnonymous) ret
        else
          designParams += dfVal
          ret + 1
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
    designDB.members.foreach {
      // checking all ports
      case dcl @ DclPort() => checkVector(dcl.dfType)
      // checking all port by name selects that change their type
      case pbns @ PortByNameSelect.Of(DFVector.Val(dclType)) if pbns.dfType != dclType =>
        vecTypeReplaceMap += pbns.dfType.asInstanceOf[DFVector] -> dclType
      // checking all assignments/connections between vectors that are considered to be similar types,
      // but are not exactly the same (e.g., two vectors types referencing a `(LEN + 1)` length value)
      case net @ DFNet(VectorNetNodeType(lhsType), _, VectorNetNodeType(rhsType), _, _, _)
          if !(lhsType == rhsType) && lhsType.isSimilarTo(rhsType) =>
        val lhsCnt = checkVector(lhsType)
        val rhsCnt = checkVector(rhsType)
        if (lhsCnt < rhsCnt) vecTypeReplaceMap += lhsType -> rhsType
        else if (lhsCnt > rhsCnt) vecTypeReplaceMap += rhsType -> lhsType
      case _ =>
    }
    val vecTypeReplacePatches = designDB.members.collect {
      case dfVal @ DFVector.Val(dfType) if vecTypeReplaceMap.contains(dfType) =>
        dfVal -> Patch.Replace(
          dfVal.updateDFType(vecTypeReplaceMap(dfType)),
          Patch.Replace.Config.FullReplacement
        )
    }
    def movedMembers(namedParam: DFVal): List[DFVal] =
      namedParam.collectRelMembers(true).filterNot(_.isGlobal)

    val addedGlobals = designParams.view.flatMap(movedMembers).toList
    val dupDesignSet = designParams.view.map(_.getOwnerDesign).toSet
    // origin -> duplicates design map
    val dupDesignMap = dupDesignSet.groupBy(_.dclName).values.map { grp =>
      val (dups, orig) = grp.partition(_.isDuplicate)
      assert(orig.size == 1)
      orig.head -> dups.toList
    }.toMap
    val dupRefTable = mutable.Map.empty[DFRefAny, DFMember]
    val dupDesignMembersMap = mutable.Map.empty[DFDesignBlock, List[DFMember]]
    // go through all designs that require member duplication from their original design
    dupDesignMap.foreach { (orig, dups) =>
      val origMembers = designDB.designMemberTable(orig)
      dups.foreach { dup =>
        val dupPublicMembers = designDB.designMemberTable(dup)
        // orig->dup replacement map memoization for reference mapping in `dupRefTable`
        val origToDupMemberMap = mutable.Map.empty[DFMember, DFMember]
        // add the designs to the replacement map
        origToDupMemberMap += orig -> dup
        // collect all public members (they are not necessarily at the top of the design)
        val origPublicMembers = origMembers.filter(_.isPublicMember)
        assert(origPublicMembers.length == dupPublicMembers.length)
        // adding public members to the orig->dup member replacement map
        origPublicMembers.lazyZip(dupPublicMembers).foreach(origToDupMemberMap += _)
        // if the replacement map has a member, get its mapped replacement, otherwise the member
        // remains as is for reference change
        def getReplacement(member: DFMember): DFMember =
          origToDupMemberMap.getOrElse(member, member)
        val dupMembers = origMembers.map { origMember =>
          // a public member already has an equivalent as a dup design member
          if (origMember.isPublicMember) origToDupMemberMap(origMember)
          // a non-public member needs to be duplicated with new references to be mapped accordingly
          else
            // duplicate member
            val dupMember = origMember.copyWithNewRefs
            // add to replacement map
            origToDupMemberMap += origMember -> dupMember
            // add duplicated member owner reference
            dupRefTable += dupMember.ownerRef -> getReplacement(origMember.getOwner)
            // add duplicated member relative references
            origMember.getRefs.lazyZip(dupMember.getRefs).foreach { (origRef, dupRef) =>
              dupRefTable += dupRef -> getReplacement(origRef.get)
            }
            dupMember
        }
        dupDesignMembersMap += dup -> dupMembers
      }
    }

    // manually the duplicated members and then using patch for replacing
    // the design parameters with global parameters
    def populateWithDupMembers(members: List[DFMember]): List[DFMember] =
      members.flatMap {
        case design: DFDesignBlock =>
          design :: populateWithDupMembers(
            dupDesignMembersMap.getOrElse(design, designDB.designMemberTable(design))
          )
        case member => Some(member)
      }
    val dupDesignDB = designDB.copy(
      members = designDB.membersGlobals ++ populateWithDupMembers(List(designDB.top)),
      refTable = designDB.refTable ++ dupRefTable
    )

    // patches to change the duplicated design declaration names to a unique identifier according
    // to theie instance names
    val designPatches = dupDesignMap.view.flatMap {
      case (orig, dups) if dups.length >= 1 =>
        (orig :: dups).map { design =>
          val updatedDclMeta =
            design.dclMeta.setName(
              s"${design.dclName}_${design.getFullName.replaceAll("\\.", "_")}"
            )
          val updatedDesign = design.removeTagOf[DuplicateTag].copy(dclMeta = updatedDclMeta)
          design -> Patch.Replace(updatedDesign, Patch.Replace.Config.FullReplacement)
        }
      case _ => None
    }.toList
    // add global parameters before the design top
    val dsn = new MetaDesign(dupDesignDB.top, Patch.Add.Config.Before):
      // patches to replace with properly named parameter or just move the anonymous members
      val replacePatches = addedGlobals.map {
        case m if !m.isAnonymous =>
          val globalParam =
            m.setName(m.getFullName.replaceAll("\\.", "_")).removeTagOf[DesignParamTag.type]
          plantMember(globalParam)
          m -> Patch.Replace(globalParam, Patch.Replace.Config.ChangeRefAndRemove)
        case m =>
          plantMember(m)
          m -> Patch.Remove(isMoved = true)
      }
    // TODO: when combined to a single patch, there is a bug that prevents some members to get a global ownership
    dupDesignDB
      .patch(dsn.patch :: dsn.replacePatches ++ vecTypeReplacePatches)
      .patch(designPatches)
  end transform
end GlobalizePortVectorParams

extension [T: HasDB](t: T)
  def globalizePortVectorParams(using CompilerOptions): DB =
    StageRunner.run(GlobalizePortVectorParams)(t.db)
