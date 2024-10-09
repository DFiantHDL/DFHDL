package dfhdl.compiler.stages

import dfhdl.compiler.analysis.*
import dfhdl.compiler.ir.*
import dfhdl.compiler.patching.*
import dfhdl.options.CompilerOptions
import scala.collection.immutable.ListMap
import dfhdl.compiler.stages.vhdl.VHDLDialect

/** This stage inlines port vector parameters that set by design arguments. This is needed only for
  * vhdl.v93 that does not support arrays with unconstrained ranges.
  * @param filterPred
  */
case object InlinePortVectorParams extends Stage:
  def dependencies: List[Stage] = List()
  def nullifies: Set[Stage] = Set(DropUnreferencedAnons)
  override def runCondition(using co: CompilerOptions): Boolean =
    co.backend match
      case be: dfhdl.backends.vhdl =>
        be.dialect match
          case VHDLDialect.v93 => true
          case _               => false
      case _ => false
  def transform(designDB: DB)(using MemberGetSet, CompilerOptions): DB =
    // match and return an inlined parameter if it's dependent on a design parameter
    object IntParamInline:
      extension (ref: DFRef.TwoWayAny)
        def isDependentOnDesignParam: Boolean =
          ref.get match
            case DesignParam(_) => true
            case dfVal          => dfVal.getRefs.exists(_.isDependentOnDesignParam)
      def unapply(intParamRef: IntParamRef): Option[IntParamRef] =
        val isDependentOnDesignParam =
          intParamRef.getRef.map(_.isDependentOnDesignParam).getOrElse(false)
        if (isDependentOnDesignParam) Some(IntParamRef(intParamRef.getInt))
        else None
    end IntParamInline
    object ComposedDFTypeReplacement:
      def unapply(dfType: DFType): Option[DFType] = dfType match
        case dt: DFVector =>
          var cellDimHasChanged = false
          // checking vector dimensions for parameters we need to inline
          val cellDimUpdate = dt.cellDimParamRefs.map {
            case IntParamInline(updatedParamRef) =>
              cellDimHasChanged = true
              updatedParamRef
            case ref => ref
          }
          // checking vector cell type for composed dependency on parameters we need to inline
          val cellTypeUpdate = dt.cellType match
            case DFBits(IntParamInline(widthParamRef)) => Some(DFBits(widthParamRef))
            case DFUInt(IntParamInline(widthParamRef)) => Some(DFUInt(widthParamRef))
            case DFSInt(IntParamInline(widthParamRef)) => Some(DFSInt(widthParamRef))
            case dt: DFVector                          => unapply(dt)
            case _                                     => None
          if (cellDimHasChanged || cellTypeUpdate.nonEmpty)
            Some(DFVector(cellTypeUpdate.get, cellDimUpdate))
          else None
        case _ => None
      end unapply
    end ComposedDFTypeReplacement
    object PortReplacement:
      def unapply(dcl: DFVal.Dcl): Option[DFVal.Dcl] =
        if (dcl.isPort)
          dcl.dfType match
            case ComposedDFTypeReplacement(dfType) => Some(dcl.updateDFType(dfType))
            case _                                 => None
        else None
    val patchList: List[(DFMember, Patch)] =
      designDB.members.collect {
        // all other opaque values need to be changed to their actual types
        case port @ PortReplacement(updatedPort) =>
          port -> Patch.Replace(updatedPort, Patch.Replace.Config.FullReplacement)
      }
    designDB.patch(patchList)
  end transform
end InlinePortVectorParams

extension [T: HasDB](t: T)
  def inlinePortVectorParams(using CompilerOptions): DB =
    StageRunner.run(InlinePortVectorParams)(t.db)
