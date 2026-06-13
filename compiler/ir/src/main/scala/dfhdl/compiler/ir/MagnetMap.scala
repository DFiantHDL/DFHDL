package dfhdl.compiler.ir
import dfhdl.internals.*
import dfhdl.compiler.analysis.*
import scala.collection.View
import scala.annotation.tailrec

enum ConnectPoint(_dfType: DFType, _dir: DFVal.Modifier.Dir) derives CanEqual:
  case Via(
      _dfType: DFType,
      _dir: DFVal.Modifier.Dir,
      designInst: DFDesignInst,
      portNamePath: String
  ) extends ConnectPoint(_dfType, _dir)
  case Direct(dcl: DFVal.Dcl) extends ConnectPoint(dcl.dfType, dcl.modifier.dir)
  val dfType: DFType = _dfType
  val dir: DFVal.Modifier.Dir = _dir
  val isPortIn: Boolean = dir == DFVal.Modifier.Dir.IN
  val isPortOut: Boolean = dir == DFVal.Modifier.Dir.OUT
  val isVar: Boolean = this match
    case Direct(dcl) => dcl.isVar
    case _           => false
  def isInsideOwner(that: DFDesignBlock)(using MemberGetSet): Boolean =
    this match
      case Direct(dcl)              => dcl.isInsideOwner(that)
      case Via(_, _, designInst, _) => designInst.isInsideOwner(that)
  def getOwnerDesign(using MemberGetSet): DFDesignBlock = this match
    case Via(_, _, designInst, _) => designInst.getDesignBlock
    case Direct(dcl)              => dcl.getOwnerDesign
  def position: Position = this match
    case Via(_, _, designInst, _) => designInst.meta.position
    case Direct(dcl)              => dcl.meta.position
  def getFullName(using MemberGetSet): String = this match
    case Via(_, _, designInst, portNamePath) =>
      s"${designInst.getFullName}.$portNamePath"
    case Direct(dcl) => dcl.getFullName
  // TODO: do we need to support creating magnets within domain blocks?
  def getName(using MemberGetSet): String = this match
    case Via(_, _, _, portNamePath) => portNamePath.replace('.', '_')
    case Direct(dcl)                => dcl.getName
  // override equals and hashCode to ignore the Via dfType that may be different across different
  // different connection point hierachies due to the ReachableType mechanism
  override def equals(that: Any): Boolean =
    (this, that) match
      case (thisVia: Via, thatVia: Via) =>
        thisVia.designInst == thatVia.designInst && thisVia.portNamePath == thatVia.portNamePath &&
        thisVia.dir == thatVia.dir
      case (Direct(dcl1), Direct(dcl2)) => dcl1 == dcl2
      case _                            => false
  override def hashCode: Int = this match
    case Via(_, dir, designInst, portNamePath) => (dir, designInst, portNamePath).hashCode()
    case Direct(dcl)                           => dcl.hashCode()
end ConnectPoint
object ConnectPoint:
  object Via:
    def apply(designInst: DFDesignInst, dcl: DFVal.Dcl)(using MemberGetSet): ConnectPoint.Via =
      ConnectPoint.Via(
        dcl.dfType,
        dcl.modifier.dir,
        designInst,
        dcl.getRelativeName(dcl.getOwnerDesign)
      )
    def apply(pbns: DFVal.PortByNameSelect)(using MemberGetSet): ConnectPoint.Via =
      ConnectPoint.Via(
        pbns.dfType,
        pbns.dir,
        pbns.getDesignInst,
        pbns.portNamePath
      )
  end Via
end ConnectPoint

//                       To          From
type MagnetMap = Map[ConnectPoint, ConnectPoint]

object MagnetMap:
  def get(using MemberGetSet): MagnetMap =
    var errors = List.empty[String]
    def newError(errMsg: String): Option[ConnectPoint] =
      errors = errMsg :: errors
      None
    val magnetPointView: View[ConnectPoint] =
      // include the magnet points that are declared via design instances
      getSet.designDB.members.view.flatMap {
        case designInst: DFDesignInst =>
          designInst.getDesignBlock.members(MemberView.Folded).view.flatMap {
            case dcl @ MagnetDcl(_) => Some(ConnectPoint.Via(designInst, dcl))
            case _                  => None
          }
        case _ => Nil
      } ++
        // also include the magnet points that are directly declared in the design,
        // not via a design instance
        getSet.designDB.designMemberList.view.flatMap { (design, members) =>
          members.view.flatMap {
            case dcl @ MagnetDcl(_) => Some(ConnectPoint.Direct(dcl))
            case _                  => None
          }
        }
    val magnetPointGrps: List[List[ConnectPoint]] = magnetPointView.groupBy {
      case cp => cp.dfType
    }.view.values.map(_.toList).toList

    // set of magnet ports that are explicitly connected/assigned
    val alreadyConnectedOrAssignedDcls: Set[DFVal.Dcl] =
      getSet.designDB.assignmentsTable.keys.flatMap(_.dealias).collect {
        case dcl @ MagnetDcl(_) if dcl.isPort => dcl
      }.toSet ++ getSet.designDB.connectionTable.connectToVals.collect {
        case dcl @ MagnetDcl(_) if dcl.isPort => dcl
      }

    val alreadyConnectedMPVias: Set[ConnectPoint.Via] =
      getSet.designDB.connectionTable.connectToVals.flatMap {
        case dfVal @ Magnet(_) => dfVal match
            case pbns: DFVal.PortByNameSelect => Some(ConnectPoint.Via(pbns))
            case _                            => None
        case _ => None
      }
    // TODO: what to do with missing clk/rst definitions in RTDomains when they are not declared?
    // Option 1: create a dedicated check for clk/rst
    // Option 2: always add clk/rst in RTDomains in elaboration using injection, if the user did not construct them.
    //           this will remove the need for AddClkRst stage.
    // Option 3: apply AddClkRst stage after elaboration and before elaboration checks. this is interesting since we could
    //           use this mechanism to apply various design fixes from elaboration meta-programming.
    def missingSourceError(targetMP: ConnectPoint): Option[ConnectPoint] =
      // newError(
      //   s"""|Missing magnet source for target port ${targetPort.getName}
      //       |Position:  ${targetPort.meta.position}
      //       |Hierarchy: ${targetPort.getOwnerNamed.getFullName}""".stripMargin
      // )
      None

    val ret = magnetPointGrps.flatMap { mpGrp =>
      mpGrp.view
        // first rejecting inviable magnet targets
        .filter {
          // rejecting design block inputs or outputs of blackbox design blocks or
          // already connected/assigned direct design blocks variables/ports
          case ConnectPoint.Direct(dcl)
              if dcl.isPortIn || dcl.isPortOut && dcl.getOwnerDesign.isBlackBox ||
                alreadyConnectedOrAssignedDcls.contains(dcl) => false
          // rejecting connected port vias that are outputs or already connected
          case via: ConnectPoint.Via if via.isPortOut || alreadyConnectedMPVias.contains(via) =>
            false
          // the rest of the points are viable magnet targets
          case _ => true
        }
        // finding the magnet source point for each target point
        .flatMap { targetMP =>
          val targetDsn = targetMP.getOwnerDesign
          val sourceMP: Option[ConnectPoint] =
            // target is a via port in (direct port in cannot be a target)
            if (targetMP.isPortIn)
              // sorted source in port candidates according to the distance
              val sourceInCandidates = mpGrp.filter {
                case ConnectPoint.Direct(dcl)
                    if (dcl.isPortIn || dcl.isVar) && targetMP.isInsideOwner(dcl.getOwnerDesign) =>
                  true
                case _ => false
              }.map { srcMP =>
                (srcMP, targetDsn.getDistanceFromOwnerDesign(srcMP.getOwnerDesign))
              }.toList.sortBy(_._2)
              // sorted source out port candidates according to the distance
              val sourceOutCandidates = mpGrp.filter {
                case via: ConnectPoint.Via if via.isPortOut => true
                case _                                      => false
              }.map { srcMP =>
                val mpDsn = srcMP.getOwnerDesign
                val commonDesign = targetDsn.getCommonDesignWith(mpDsn)
                (
                  srcMP,
                  targetDsn.getDistanceFromOwnerDesign(commonDesign),
                  mpDsn.getDistanceFromOwnerDesign(commonDesign)
                )
              }.toList.sortBy(_._3).sortBy(_._2)
              (sourceInCandidates, sourceOutCandidates) match
                case (Nil, Nil) =>
                  missingSourceError(targetMP)
                case (Nil, (src, _, _) :: _) =>
                  Some(src)
                case ((src, _) :: _, Nil) =>
                  Some(src)
                case ((srcIn, distIn) :: _, (srcOut, distOut, _) :: _) =>
                  if (distIn < distOut) Some(srcIn)
                  else
                    newError(
                      s"""|Found two possible magnet sources for a target magnet.
                          |Target Position:  ${targetMP.position}
                          |Target Path:      ${targetMP.getFullName}
                          |Source1 Position: ${srcIn.position} 
                          |Source1 Path:     ${srcIn.getFullName}
                          |Source2 Position: ${srcOut.position} 
                          |Source2 Path:     ${srcOut.getFullName}""".stripMargin
                    )
              end match
            // target is direct output port
            else
              // sorted source candidates according to the distance
              val sourceOutCandidates = mpGrp.filter {
                case via: ConnectPoint.Via if via.isPortOut && via.isInsideOwner(targetDsn) => true
                case direct: ConnectPoint.Direct
                    if direct.isVar && direct.isInsideOwner(targetDsn) ||
                      direct.isPortIn && direct.getOwnerDesign == targetDsn => true
                case _ => false
              }.map { srcMP =>
                (srcMP, srcMP.getOwnerDesign.getDistanceFromOwnerDesign(targetDsn))
              }.toList.sortBy(_._2)
              sourceOutCandidates match
                case Nil =>
                  missingSourceError(targetMP)
                case (src, ld) :: otherCandidates =>
                  var lastDistance: Int = ld
                  var lastSrc: ConnectPoint = src
                  otherCandidates.foreach { case (src, distance) =>
                    if (distance == lastDistance)
                      newError(
                        s"""|Found two possible magnet sources for a target magnet.
                            |Target Position:  ${targetMP.position}
                            |Target Path:      ${targetMP.getFullName}
                            |Source1 Position: ${lastSrc.position} 
                            |Source1 Path:     ${lastSrc.getFullName}
                            |Source2 Position: ${src.position} 
                            |Source2 Path:     ${src.getFullName}""".stripMargin
                      )
                    lastDistance = distance
                    lastSrc = src
                  }
                  Some(src)
              end match
            end if
          end sourceMP
          sourceMP.map(targetMP -> _)
        }
    }.toMap
    if (errors.nonEmpty)
      throw new IllegalArgumentException(
        errors.view.reverse.mkString("\n\n")
      )
    ret
  end get

  // Hierarchical (new-style root DB) clone of `get`. Magnet matching is intrinsically
  // cross-design: it pairs sources to targets across the whole hierarchy by distance.
  // Approach: resolve each magnet ConnectPoint's design context (owner + container
  // design) ONCE under its owning sub-DB getSet, then run the distance / inside-owner
  // matching purely on the root-aware design tree (designBlockOwnershipMap) — no ref
  // resolution during matching, so the throwing root getSet is fine.
  def getHierarchical(rootDB: DB): (MagnetMap, Map[ConnectPoint, (DFDesignBlock, String)]) =
    // a magnet ConnectPoint with its design context precomputed under the
    // owning sub-DB getSet (so the matching never resolves refs)
    final case class RMP(
        cp: ConnectPoint,
        ownerDesign: DFDesignBlock, // cp.getOwnerDesign
        containerDesign: DFDesignBlock, // design directly containing the point's member
        ownerIsBlackBox: Boolean,
        name: String,
        fullName: String,
        position: Position
    ):
      def dfType: DFType = cp.dfType
      def isPortIn: Boolean = cp.isPortIn
      def isPortOut: Boolean = cp.isPortOut
      def isVar: Boolean = cp.isVar

    var errors = List.empty[String]
    def newError(errMsg: String): Option[RMP] =
      errors = errMsg :: errors
      None

    // Via magnet points: a design instance's magnet ports. The inst lives in a
    // parent sub-DB; its magnet ports live in the instantiated child design's
    // sub-DB — resolve each under the right getSet.
    val viaRMPs: List[RMP] = rootDB.subDBs.values.iterator.flatMap { parentSub =>
      parentSub.atGetSet {
        parentSub.membersNoGlobals.iterator.collect { case inst: DFDesignInst => inst }.flatMap {
          inst =>
            val childDesign = inst.getDesignBlock
            val containerDesign = inst.getOwnerDesign
            val instFullName = inst.getFullName
            val instPos = inst.meta.position
            rootDB.subDBs.get(childDesign.ownerRef).iterator.flatMap { childSub =>
              childSub.atGetSet {
                childDesign.members(MemberView.Folded).iterator.collect {
                  case dcl @ MagnetDcl(_) =>
                    val cp = ConnectPoint.Via(inst, dcl)
                    RMP(
                      cp,
                      childDesign,
                      containerDesign,
                      childDesign.isBlackBox,
                      cp.getName,
                      s"$instFullName.${dcl.getRelativeName(childDesign)}",
                      instPos
                    )
                }
              }
            }
        }
      }
    }.toList

    // Direct magnet points: magnet ports/vars declared directly in a design.
    val directRMPs: List[RMP] = rootDB.subDBs.values.iterator.flatMap { subDB =>
      subDB.atGetSet {
        subDB.membersNoGlobals.iterator.collect {
          case dcl @ MagnetDcl(_) =>
            val cp = ConnectPoint.Direct(dcl)
            val ownerDesign = dcl.getOwnerDesign
            RMP(cp, ownerDesign, ownerDesign, ownerDesign.isBlackBox, dcl.getName,
              dcl.getFullName, dcl.meta.position)
        }
      }
    }.toList

    val allRMPs: List[RMP] = viaRMPs ++ directRMPs

    // magnet ports already explicitly connected/assigned (per sub-DB connectivity, unioned)
    val alreadyConnectedOrAssignedDcls: Set[DFVal.Dcl] =
      rootDB.subDBs.values.iterator.flatMap { subDB =>
        subDB.atGetSet {
          subDB.assignmentsTable.keys.flatMap(_.dealias).collect {
            case dcl @ MagnetDcl(_) if dcl.isPort => dcl
          } ++ subDB.connectionTable.connectToVals.collect {
            case dcl @ MagnetDcl(_) if dcl.isPort => dcl
          }
        }
      }.toSet
    val alreadyConnectedMPVias: Set[ConnectPoint.Via] =
      rootDB.subDBs.values.iterator.flatMap { subDB =>
        subDB.atGetSet {
          subDB.connectionTable.connectToVals.flatMap {
            case dfVal @ Magnet(_) =>
              dfVal match
                case pbns: DFVal.PortByNameSelect => Some(ConnectPoint.Via(pbns))
                case _                            => None
            case _ => None
          }
        }
      }.toSet

    // is `x` the same design as `anc`, or a descendant of it, in the design tree
    @tailrec def reaches(
        frontier: Set[DFDesignBlock],
        anc: DFDesignBlock,
        seen: Set[DFDesignBlock]
    ): Boolean =
      if (frontier.isEmpty) false
      else if (frontier.contains(anc)) true
      else
        reaches(
          frontier.flatMap(rootDB.designBlockOwnershipMap.getOrElse(_, Set.empty)) -- seen,
          anc,
          seen ++ frontier
        )
    def isDescendantOrSelf(x: DFDesignBlock, anc: DFDesignBlock): Boolean =
      (x == anc) || reaches(rootDB.designBlockOwnershipMap.getOrElse(x, Set.empty), anc, Set(x))

    // root getSet: the distance helpers only read designBlockOwnershipMap (root-aware)
    // and never resolve refs on a DFDesignBlock, so this never throws.
    given MemberGetSet = rootDB.getSet

    val groups: List[List[RMP]] = allRMPs.groupBy(_.dfType).values.map(_.toList).toList
    val ret = groups.flatMap { grp =>
      grp.view.filter { rmp =>
        rmp.cp match
          case ConnectPoint.Direct(dcl)
              if rmp.isPortIn || rmp.isPortOut && rmp.ownerIsBlackBox ||
                alreadyConnectedOrAssignedDcls.contains(dcl) =>
            false
          case via: ConnectPoint.Via if rmp.isPortOut || alreadyConnectedMPVias.contains(via) =>
            false
          case _ => true
      }.flatMap { targetRMP =>
        val targetDsn = targetRMP.ownerDesign
        val sourceRMP: Option[RMP] =
          if (targetRMP.isPortIn)
            val sourceInCandidates = grp.filter { c =>
              c.cp match
                case ConnectPoint.Direct(_)
                    if (c.isPortIn || c.isVar) &&
                      isDescendantOrSelf(targetRMP.containerDesign, c.ownerDesign) =>
                  true
                case _ => false
            }.map(src => (src, targetDsn.getDistanceFromOwnerDesign(src.ownerDesign)))
              .sortBy(_._2)
            val sourceOutCandidates = grp.filter { c =>
              c.cp match
                case _: ConnectPoint.Via if c.isPortOut => true
                case _                                  => false
            }.map { src =>
              val mpDsn = src.ownerDesign
              val commonDesign = targetDsn.getCommonDesignWith(mpDsn)
              (
                src,
                targetDsn.getDistanceFromOwnerDesign(commonDesign),
                mpDsn.getDistanceFromOwnerDesign(commonDesign)
              )
            }.sortBy(_._3).sortBy(_._2)
            (sourceInCandidates, sourceOutCandidates) match
              case (Nil, Nil)                                        => None
              case (Nil, (src, _, _) :: _)                           => Some(src)
              case ((src, _) :: _, Nil)                              => Some(src)
              case ((srcIn, distIn) :: _, (srcOut, distOut, _) :: _) =>
                if (distIn < distOut) Some(srcIn)
                else
                  newError(
                    s"""|Found two possible magnet sources for a target magnet.
                        |Target Position:  ${targetRMP.position}
                        |Target Path:      ${targetRMP.fullName}
                        |Source1 Position: ${srcIn.position}
                        |Source1 Path:     ${srcIn.fullName}
                        |Source2 Position: ${srcOut.position}
                        |Source2 Path:     ${srcOut.fullName}""".stripMargin
                  )
            end match
          else
            val sourceOutCandidates = grp.filter { c =>
              c.cp match
                case _: ConnectPoint.Via
                    if c.isPortOut && isDescendantOrSelf(c.containerDesign, targetDsn) =>
                  true
                case _: ConnectPoint.Direct
                    if c.isVar && isDescendantOrSelf(c.containerDesign, targetDsn) ||
                      c.isPortIn && (c.ownerDesign == targetDsn) =>
                  true
                case _ => false
            }.map(src => (src, src.ownerDesign.getDistanceFromOwnerDesign(targetDsn)))
              .sortBy(_._2)
            sourceOutCandidates match
              case Nil                          => None
              case (src, ld) :: otherCandidates =>
                var lastDistance: Int = ld
                var lastSrc: RMP = src
                otherCandidates.foreach { case (s, distance) =>
                  if (distance == lastDistance)
                    newError(
                      s"""|Found two possible magnet sources for a target magnet.
                          |Target Position:  ${targetRMP.position}
                          |Target Path:      ${targetRMP.fullName}
                          |Source1 Position: ${lastSrc.position}
                          |Source1 Path:     ${lastSrc.fullName}
                          |Source2 Position: ${s.position}
                          |Source2 Path:     ${s.fullName}""".stripMargin
                    )
                  lastDistance = distance
                  lastSrc = s
                }
                Some(src)
            end match
        sourceRMP.map(s => targetRMP.cp -> s.cp)
      }
    }.toMap
    if (errors.nonEmpty)
      throw new IllegalArgumentException(errors.view.reverse.mkString("\n\n"))
    val pointInfo: Map[ConnectPoint, (DFDesignBlock, String)] =
      allRMPs.iterator.map(rmp => rmp.cp -> (rmp.ownerDesign, rmp.name)).toMap
    (ret, pointInfo)
  end getHierarchical
end MagnetMap
