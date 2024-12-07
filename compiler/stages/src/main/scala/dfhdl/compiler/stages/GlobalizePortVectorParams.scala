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
    val patchList = designParams.lazyZip(globalParams).map((dp, gp) =>
      dp -> Patch.Replace(gp, Patch.Replace.Config.ChangeRefAndRemove)
    ).toList
    // manually adding the global members and then using patch for replacing
    // the design parameters with global parameters
    val updatedMembers = globalParams ++ designDB.members
    val updatedRefTable = designDB.refTable ++ globalsDB.refTable
    designDB.copy(updatedMembers, updatedRefTable).patch(patchList)
  end transform
end GlobalizePortVectorParams

extension [T: HasDB](t: T)
  def globalizePortVectorParams(using CompilerOptions): DB =
    StageRunner.run(GlobalizePortVectorParams)(t.db)
