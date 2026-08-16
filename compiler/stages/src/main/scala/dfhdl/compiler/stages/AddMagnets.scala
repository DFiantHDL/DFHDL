package dfhdl.compiler.stages

import dfhdl.compiler.analysis.*
import dfhdl.compiler.ir.*
import dfhdl.compiler.patching.*
import dfhdl.internals.*
import dfhdl.options.CompilerOptions
import scala.collection.mutable
import scala.collection.immutable.ListMap
import dfhdl.core.DFOpaque as coreDFOpaque
import dfhdl.core.{asFE, ModifierAny}
import DFVal.Modifier.Dir.{IN, OUT}

/** This stage adds missing magnet ports across the entire design. These will be connected at a
  * later stage.
  */
case object AddMagnets extends GlobalStage:
  def dependencies: List[Stage] = List(AddClkRst)
  def nullifies: Set[Stage] = Set(ViaConnection, SimpleOrderMembers)
  def transformGlobal(designDB: DB)(using co: CompilerOptions, refGen: RefGen): DB =
    // root getSet: only `getCommonDesignWith` uses it, and only on DFDesignBlocks
    // (reads designBlockOwnershipMap, never resolves a ref), so it never throws.
    given MemberGetSet = designDB.getSet
    // owner design + name of each magnet point, precomputed cross-design by the
    // analysis so a ConnectPoint living in another sub-DB is never re-resolved.
    def ownerOf(cp: ConnectPoint): DFDesignBlock = designDB.magnetPointInfo(cp)._1
    def nameOf(cp: ConnectPoint): String = designDB.magnetPointInfo(cp)._2
    // Populating a missing magnets map with the suggested port names and direction
    val missingMagnets = mutable.Map.empty[DFDesignBlock, Map[DFType, (String, DFVal.Modifier.Dir)]]
    def registerMissingMagnet(
        dsn: DFDesignBlock,
        dfType: DFType,
        name: String,
        dir: DFVal.Modifier.Dir
    ): Unit =
      missingMagnets.get(dsn) match
        case None => missingMagnets += dsn -> Map(dfType -> (name, dir))
        case Some(dfTypeNameMap) if !dfTypeNameMap.contains(dfType) =>
          missingMagnets += dsn -> (dfTypeNameMap + (dfType -> (name, dir)))
        case _ => // do nothing
    // climb from a bottom design upward while memoizing missing magnets. With
    // DFDesignBlock.ownerRef == Empty the lexical parent is no longer reachable via
    // getOwnerDesign — walk up through `designBlockOwnershipMap` (parents-via-instances,
    // root-aware) instead. Multiple parents at any level are all visited; iteration stops
    // once we reach `topDsnOpt` (exclusive; None climbs through the entire hierarchy,
    // top design included). The bottom endpoint is excluded from the registration.
    def climbUpDsn(
        bottomDsn: DFDesignBlock,
        topDsnOpt: Option[DFDesignBlock],
        dfType: DFType,
        name: String,
        dir: DFVal.Modifier.Dir
    ): Unit =
      val visited = mutable.Set.empty[DFDesignBlock]
      val queue = mutable.Queue.empty[DFDesignBlock]
      queue ++= designDB.designBlockOwnershipMap.getOrElse(bottomDsn, Set.empty)
      while (queue.nonEmpty)
        val dsn = queue.dequeue()
        if (!topDsnOpt.contains(dsn) && visited.add(dsn))
          registerMissingMagnet(dsn, dfType, name, dir)
          queue ++= designDB.designBlockOwnershipMap.getOrElse(dsn, Set.empty)
    end climbUpDsn
    designDB.magnetConnectionMap.foreach { (toMP, fromMP) =>
      val toDsn = ownerOf(toMP)
      val fromDsn = ownerOf(fromMP)
      val fromName = nameOf(fromMP)
      val dfType = toMP.dfType
      def climbUp(bottomPort: ConnectPoint, topDsn: DFDesignBlock): Unit =
        climbUpDsn(ownerOf(bottomPort), Some(topDsn), dfType, fromName, bottomPort.dir)
      (toMP.dir, fromMP.dir) match
        // climbing up to the source input port
        case (IN, IN) => climbUp(toMP, fromDsn)
        // climbing up to the target output port
        case (OUT, OUT) => climbUp(fromMP, toDsn)
        // climbing up from the output source and up from the input target to the common design
        case (IN, OUT) =>
          val commonDsn = toDsn.getCommonDesignWith(fromDsn)
          if (commonDsn != fromDsn)
            climbUp(fromMP, commonDsn)
          if (commonDsn != toDsn)
            climbUp(toMP, commonDsn)
        case _ => // do nothing
      end match
    }
    // Sourceless derived clocks: a Clk-kind magnet target with no source anywhere in the
    // hierarchy climbs all the way up, top design included, so the derived clock surfaces
    // as a top-level input port instead of dangling internally (a forgotten gated-clock
    // connection becomes a visible port rather than a silently dead clock). ConnectMagnets
    // recomputes the magnet map on the patched DB, where the top-added port is the source
    // that wires the whole chain. Root clock groups never reach here: AddClkRst gives every
    // design that uses them its own dcl, which is the subtree's source.
    designDB.magnetUnmatchedTargets.foreach { toMP =>
      toMP.dfType match
        case DFOpaque(kind = DFOpaque.Kind.Clk) if toMP.isPortIn =>
          climbUpDsn(ownerOf(toMP), None, toMP.dfType, nameOf(toMP), DFVal.Modifier.Dir.IN)
        case _ =>
    }
    // Add each design's missing magnet ports under that design's own sub-DB getSet.
    val newSubDBs = ListMap.from(
      designDB.subDBs.iterator.map { case (key, subDB) =>
        val design = subDB.top
        missingMagnets.get(design) match
          case Some(magnetsMap) =>
            // sorting added magnets for consistent port addition order
            val magnets = magnetsMap.toList.sortBy(_._2._1)
            val dsn = subDB.atGetSet {
              new MetaDesign(design, Patch.Add.Config.InsideFirst):
                for ((dfType, (name, dir)) <- magnets)
                  val modFE = new ModifierAny(DFVal.Modifier(dir, DFVal.Modifier.Special.Ordinary))
                  // the ports are added as first members
                  (dfType.asFE[DFType] <> modFE)(using dfc.setName(name))
            }
            key -> subDB.patch(List(dsn.patch))
          case None => key -> subDB
      }
    )
    designDB.update(subDBs = newSubDBs)
  end transformGlobal
end AddMagnets

extension [T: HasDB](t: T)
  def addMagnets(using CompilerOptions): DB = StageRunner.run(AddMagnets)(t.db)
