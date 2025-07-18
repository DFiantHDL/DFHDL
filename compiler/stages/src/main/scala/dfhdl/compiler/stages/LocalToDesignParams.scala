package dfhdl.compiler.stages

import dfhdl.compiler.analysis.*
import dfhdl.compiler.ir.*
import dfhdl.compiler.patching.*
import dfhdl.options.CompilerOptions
import scala.collection.mutable

/** This stage converts local parameters that are used in IOs to be design parameters, since VHDL
  * does not support local parameters for IO access.
  */
case object LocalToDesignParams extends Stage:
  override def runCondition(using co: CompilerOptions): Boolean =
    co.backend match
      case be: dfhdl.backends.vhdl => true
      case _                       => false
  override def dependencies: List[Stage] = List(GlobalizePortVectorParams)
  override def nullifies: Set[Stage] = Set()
  def transform(designDB: DB)(using getSet: MemberGetSet, co: CompilerOptions): DB =
    given RefGen = RefGen.fromGetSet
    val ioLocalParams = mutable.LinkedHashSet.empty[DFVal]
    def collectIOLocalParams(ref: DFRefAny): Unit = ref.get match
      // skip existing design parameters
      case _: DFVal.DesignParam => // do nothing
      // skip global parameters
      case gp: DFVal.CanBeGlobal if gp.isGlobal => // do nothing
      // check this value and its dependencies
      case dfVal: DFVal =>
        // already collected
        if (!ioLocalParams.contains(dfVal))
          // collect dependencies
          dfVal.getRefs.foreach(collectIOLocalParams)
          // if the value is named collect this value too
          if (!dfVal.isAnonymous)
            ioLocalParams.add(dfVal)
      case _ => // do nothing

    // collect all local parameters that are used in IOs
    designDB.members.foreach {
      case port @ DclPort() => port.getRefs.foreach(collectIOLocalParams)
      case _                => // do nothing
    }

    // adding design parameters with initial values set as the anonymized local parameters
    val patchList: List[(DFMember, Patch)] =
      ioLocalParams.view.collect { lp =>
        val dsn = new MetaDesign(
          lp,
          Patch.Add.Config.ReplaceWithLast(Patch.Replace.Config.FullReplacement)
        ):
          val lpAnon = plantMember(lp.anonymize).asValAny
          val dp = dfhdl.core.DFVal.DesignParam(lpAnon, Some(lpAnon))(using
            dfc.setMeta(lp.meta)
          )
        dsn.patch
      }.toList
    designDB.patch(patchList)
  end transform
end LocalToDesignParams

extension [T: HasDB](t: T)
  def localToDesignParams(using co: CompilerOptions): DB =
    StageRunner.run(LocalToDesignParams)(t.db)
