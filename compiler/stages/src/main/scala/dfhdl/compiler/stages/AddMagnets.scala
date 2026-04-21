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
case object AddMagnets extends Stage:
  def dependencies: List[Stage] = List(AddClkRst)
  def nullifies: Set[Stage] = Set(ViaConnection, SimpleOrderMembers)
  def transform(designDB: DB)(using MemberGetSet, CompilerOptions): DB =
    given RefGen = RefGen.fromGetSet
    // Populating a missing magnets map with the suggested port names and direction
    val missingMagnets = mutable.Map.empty[DFDesignBlock, Map[DFType, (String, DFVal.Modifier)]]
    designDB.magnetConnectionTable.foreach { (toPort, fromPort) =>
      val toPortDsn = toPort.getOwnerDesign
      val fromPortDsn = fromPort.getOwnerDesign
      val dfType = toPort.dfType
      def anotherMissingMagnet(dsn: DFDesignBlock, mod: DFVal.Modifier): Unit =
        missingMagnets.get(dsn) match
          case None => missingMagnets += dsn -> Map(dfType -> (fromPort.getName, mod))
          case Some(dfTypeNameMap) if !dfTypeNameMap.contains(dfType) =>
            missingMagnets += dsn -> (dfTypeNameMap + (dfType -> (fromPort.getName, mod)))
          case _ => // do nothing
      // climb from a bottom design to a top design, while memoizing missing
      // magnets between the designs
      def climbUpDsn(bottomDsn: DFDesignBlock, topDsn: DFDesignBlock, mod: DFVal.Modifier): Unit =
        var dsn = bottomDsn
        while (!dsn.isOneLevelBelow(topDsn))
          dsn = dsn.getOwnerDesign
          anotherMissingMagnet(dsn, mod)
      def climbUp(bottomPort: DFVal.Dcl, topDsn: DFDesignBlock): Unit =
        climbUpDsn(bottomPort.getOwnerDesign, topDsn, bottomPort.modifier)
      (toPort, fromPort) match
        // climbing up to the source input port
        case (DclIn(), DclIn()) => climbUp(toPort, fromPortDsn)
        // climbing up to the target output port
        case (DclOut(), DclOut()) => climbUp(fromPort, toPortDsn)
        // climbing up from the output source and up from the input target to the common design
        case (DclIn(), DclOut()) =>
          val commonDsn = toPortDsn.getCommonDesignWith(fromPortDsn)
          if (commonDsn != fromPortDsn)
            climbUp(fromPort, commonDsn)
          if (commonDsn != toPortDsn)
            climbUp(toPort, commonDsn)
        case _ => // do nothing
      end match
    }
    val patchList: List[(DFMember, Patch)] = designDB.designMemberList.flatMap {
      // for all designs
      case (design, _) if missingMagnets.contains(design) =>
        // sorting added magnets for consistent port addition order
        val magnets = missingMagnets(design).toList.sortBy(_._2._1)
        val dsn = new MetaDesign(design, Patch.Add.Config.InsideFirst):
          for ((dfType, (name, mod)) <- magnets)
            val modFE = new ModifierAny(mod)
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
