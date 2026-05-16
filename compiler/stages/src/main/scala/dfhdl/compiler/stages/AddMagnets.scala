package dfhdl.compiler.stages

import dfhdl.compiler.analysis.*
import dfhdl.compiler.ir.*
import dfhdl.compiler.patching.*
import dfhdl.internals.*
import dfhdl.options.CompilerOptions
import scala.collection.mutable
import dfhdl.core.DFOpaque as coreDFOpaque
import dfhdl.core.{asFE, ModifierAny}
import DFVal.Modifier.Dir.{IN, OUT}

/** This stage adds missing magnet ports across the entire design. These will be connected at a
  * later stage.
  */
case object AddMagnets extends Stage:
  def dependencies: List[Stage] = List(AddClkRst)
  def nullifies: Set[Stage] = Set(ViaConnection, SimpleOrderMembers)
  def transform(designDB: DB)(using MemberGetSet, CompilerOptions): DB =
    given RefGen = RefGen.fromGetSet
    // Populating a missing magnets map with the suggested port names and direction
    val missingMagnets = mutable.Map.empty[DFDesignBlock, Map[DFType, (String, DFVal.Modifier.Dir)]]
    designDB.magnetConnectionMap.foreach { (toMP, fromMP) =>
      val toDsn = toMP.getOwnerDesign
      val fromDsn = fromMP.getOwnerDesign
      val dfType = toMP.dfType
      def anotherMissingMagnet(dsn: DFDesignBlock, dir: DFVal.Modifier.Dir): Unit =
        missingMagnets.get(dsn) match
          case None => missingMagnets += dsn -> Map(dfType -> (fromMP.getName, dir))
          case Some(dfTypeNameMap) if !dfTypeNameMap.contains(dfType) =>
            missingMagnets += dsn -> (dfTypeNameMap + (dfType -> (fromMP.getName, dir)))
          case _ => // do nothing
      // climb from a bottom design to a top design, while memoizing missing
      // magnets between the designs. With DFDesignBlock.ownerRef == Empty the
      // lexical parent is no longer reachable via getOwnerDesign — walk up
      // through `designBlockOwnershipMap` (parents-via-instances) instead.
      // Multiple parents at any level are all visited; iteration stops once
      // we reach `topDsn`. Both endpoints are excluded from the registration.
      def climbUpDsn(
          bottomDsn: DFDesignBlock,
          topDsn: DFDesignBlock,
          dir: DFVal.Modifier.Dir
      ): Unit =
        val visited = mutable.Set.empty[DFDesignBlock]
        val queue = mutable.Queue.empty[DFDesignBlock]
        queue ++= designDB.designBlockOwnershipMap.getOrElse(bottomDsn, Set.empty)
        while (queue.nonEmpty)
          val dsn = queue.dequeue()
          if (dsn != topDsn && visited.add(dsn))
            anotherMissingMagnet(dsn, dir)
            queue ++= designDB.designBlockOwnershipMap.getOrElse(dsn, Set.empty)
      def climbUp(bottomPort: ConnectPoint, topDsn: DFDesignBlock): Unit =
        climbUpDsn(bottomPort.getOwnerDesign, topDsn, bottomPort.dir)
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
    val patchList: List[(DFMember, Patch)] = designDB.designMemberList.flatMap {
      // for all designs
      case (design, _) if missingMagnets.contains(design) =>
        // sorting added magnets for consistent port addition order
        val magnets = missingMagnets(design).toList.sortBy(_._2._1)
        val dsn = new MetaDesign(design, Patch.Add.Config.InsideFirst):
          for ((dfType, (name, dir)) <- magnets)
            val modFE = new ModifierAny(DFVal.Modifier(dir, DFVal.Modifier.Special.Ordinary))
            (dfType.asFE[DFType] <> modFE)(using dfc.setName(name))
        // the ports are added as first members
        Some(dsn.patch)
      case _ => Nil
    }
    designDB.patch(patchList)
  end transform
end AddMagnets

extension [T: HasDB](t: T)
  def addMagnets(using CompilerOptions): DB = StageRunner.run(AddMagnets)(t.db)
