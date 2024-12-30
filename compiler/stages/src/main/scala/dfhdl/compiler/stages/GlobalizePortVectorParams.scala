package dfhdl.compiler.stages

import dfhdl.compiler.analysis.*
import dfhdl.compiler.ir.*
import dfhdl.compiler.patching.*
import dfhdl.options.CompilerOptions
import scala.collection.immutable.ListMap
import scala.collection.mutable
import dfhdl.compiler.stages.vhdl.VHDLDialect
import dfhdl.core.{DFTypeAny, asFE}

/** This stage globalizes design parameters that set port vector lengths. This is needed only for
  * vhdl.v93 that does not support arrays with unconstrained ranges.
  */
case object GlobalizePortVectorParams extends Stage:
  def dependencies: List[Stage] = List(UniqueDesigns)
  def nullifies: Set[Stage] = Set(DropUnreferencedAnons)
  override def runCondition(using co: CompilerOptions): Boolean =
    co.backend match
      case be: dfhdl.backends.vhdl =>
        be.dialect match
          case VHDLDialect.v93 => true
          case _               => false
      case _ => false
  def transform(designDB: DB)(using MemberGetSet, CompilerOptions): DB =
    // to collect unique design parameters while maintaining order for consistent compilation
    val designParams = mutable.LinkedHashSet.empty[DFVal]
    // check ref
    def checkRef(ref: DFRef.TwoWayAny): Unit =
      ref.get match
        case p @ DesignParam(_) => designParams += p
        case dfVal              => dfVal.getRefs.foreach(checkRef)
    def checkIntParamRef(intParamRef: IntParamRef): Unit =
      intParamRef.getRef.foreach(checkRef)
    // checking vector types
    def checkVector(dfType: DFType): Unit = dfType match
      case dt: DFVector =>
        // checking vector dimensions for parameters we need to globalize
        val cellDimUpdate = dt.cellDimParamRefs.foreach(checkIntParamRef)
        // checking vector cell type for composed dependency on parameters we need to globalize
        dt.cellType match
          case DFBits(widthParamRef) => checkIntParamRef(widthParamRef)
          case DFUInt(widthParamRef) => checkIntParamRef(widthParamRef)
          case DFSInt(widthParamRef) => checkIntParamRef(widthParamRef)
          case dt: DFVector          => checkVector(dt.cellType)
          case _                     => None
      case _ =>
    // checking all ports
    designDB.members.foreach {
      case dcl @ DclPort() => checkVector(dcl.dfType)
      case _               =>
    }
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
    // an empty context to add the global parameters to
    val dfc = dfhdl.core.DFC.empty
    // adding global parameters to the context with the full name of the design parameters and their value
    designParams.foreach { p =>
      val updatedMeta = p.meta.setName(p.getFullName.replaceAll("\\.", "_"))
      dfhdl.core.DFVal.Const.forced(
        p.dfType.asFE[DFTypeAny],
        p.getConstData.get,
        named = true
      )(using dfc.setMeta(updatedMeta)).asIR
    }
    val globalsDB = dfc.mutableDB.immutable
    val globalParams = globalsDB.members

    // manually adding the global members and the duplicated members and then using patch for replacing
    // the design parameters with global parameters
    def populateWithDupMembers(members: List[DFMember]): List[DFMember] =
      members.flatMap {
        case design: DFDesignBlock =>
          design :: populateWithDupMembers(
            dupDesignMembersMap.getOrElse(design, designDB.designMemberTable(design))
          )
        case member => Some(member)
      }
    val updatedMembers =
      globalParams ++ designDB.membersGlobals ++ populateWithDupMembers(List(designDB.top))
    val updatedRefTable = designDB.refTable ++ dupRefTable ++ globalsDB.refTable

    val patchList = Iterable(
      designParams.lazyZip(globalParams).map((dp, gp) =>
        dp -> Patch.Replace(gp, Patch.Replace.Config.ChangeRefAndRemove)
      ),
      dupDesignMap.view.flatMap {
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
      }
    ).flatten.toList
    designDB.copy(updatedMembers, updatedRefTable).patch(patchList)
  end transform
end GlobalizePortVectorParams

extension [T: HasDB](t: T)
  def globalizePortVectorParams(using CompilerOptions): DB =
    StageRunner.run(GlobalizePortVectorParams)(t.db)
