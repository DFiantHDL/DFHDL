package dfhdl.compiler.stages

import dfhdl.compiler.analysis.*
import dfhdl.compiler.ir.*
import dfhdl.compiler.patching.*
import dfhdl.internals.*
import dfhdl.options.CompilerOptions
import scala.collection.mutable
import dfhdl.core.DFOpaque as coreDFOpaque
import dfhdl.core.{asFE, ModifierAny}

/** This stage adds missing magnet ports across the entire design. These will be connected at a
  * later stage.
  */
case object AddMagnets extends HierarchyStage:
  def dependencies: List[Stage] = List(AddClkRst)
  def nullifies: Set[Stage] = Set(ViaConnection, SimpleOrderMembers)
  // Magnet discovery (climbing design hierarchy via magnetConnectionTable)
  // and port-planting both call `getOwnerDesign` on cross-design Dcls, which
  // only resolves against the flat refTable. Run with the outer flat getSet.
  override def rebindGetSet: Boolean = false

  // Per-run cache: computed once in `transform` (the outer entry point) and
  // consumed by each `transformSubDB` call. Reset at entry so the case
  // object singleton doesn't leak across runs.
  private var cachedDesignMagnets
      : Map[DFDesignBlock, Map[DFType, (String, DFVal.Modifier)]] = Map.empty

  override def transform(designDB: DB)(using MemberGetSet, CompilerOptions): DB =
    val missing = mutable.Map.empty[DFDesignBlock, Map[DFType, (String, DFVal.Modifier)]]
    designDB.magnetConnectionTable.foreach { (toPort, fromPort) =>
      val toPortDsn = toPort.getOwnerDesign
      val fromPortDsn = fromPort.getOwnerDesign
      val dfType = toPort.dfType
      def anotherMissingMagnet(dsn: DFDesignBlock, mod: DFVal.Modifier): Unit =
        missing.get(dsn) match
          case None => missing += dsn -> Map(dfType -> (fromPort.getName, mod))
          case Some(dfTypeNameMap) if !dfTypeNameMap.contains(dfType) =>
            missing += dsn -> (dfTypeNameMap + (dfType -> (fromPort.getName, mod)))
          case _ => // do nothing
      def climbUpDsn(bottomDsn: DFDesignBlock, topDsn: DFDesignBlock, mod: DFVal.Modifier): Unit =
        var dsn = bottomDsn
        while (!dsn.isOneLevelBelow(topDsn))
          dsn = dsn.getOwnerDesign
          anotherMissingMagnet(dsn, mod)
      def climbUp(bottomPort: DFVal.Dcl, topDsn: DFDesignBlock): Unit =
        climbUpDsn(bottomPort.getOwnerDesign, topDsn, bottomPort.modifier)
      (toPort, fromPort) match
        case (DclIn(), DclIn())   => climbUp(toPort, fromPortDsn)
        case (DclOut(), DclOut()) => climbUp(fromPort, toPortDsn)
        case (DclIn(), DclOut()) =>
          val commonDsn = toPortDsn.getCommonDesignWith(fromPortDsn)
          if (commonDsn != fromPortDsn) climbUp(fromPort, commonDsn)
          if (commonDsn != toPortDsn) climbUp(toPort, commonDsn)
        case _ => // do nothing
      end match
    }
    cachedDesignMagnets = missing.toMap
    try super.transform(designDB)
    finally cachedDesignMagnets = Map.empty

  def transformSubDB(subDB: DB)(using
      getSet: MemberGetSet, co: CompilerOptions, rg: RefGen
  ): DB =
    // Each sub-DB patches ONLY its own design. Skip root (whose designBlock
    // aliases the top sub-DB's designBlock — that sub-DB handles it).
    val designOpt = subDB.designBlock.filterNot(subDB.internalDBs.contains)
    designOpt.flatMap(cachedDesignMagnets.get) match
      case Some(dfTypeMap) =>
        val design = designOpt.get
        val magnets = dfTypeMap.toList.sortBy(_._2._1)
        val dsn = new MetaDesign(design, Patch.Add.Config.InsideFirst):
          for ((dfType, (name, mod)) <- magnets)
            val modFE = new ModifierAny(mod)
            (dfType.asFE[DFType] <> modFE)(using dfc.setName(name))
        subDB.patch(List(dsn.patch))
      case None => subDB
  end transformSubDB
end AddMagnets

extension [T: HasDB](t: T)
  def addMagnets(using CompilerOptions): DB = StageRunner.run(AddMagnets)(t.db)
