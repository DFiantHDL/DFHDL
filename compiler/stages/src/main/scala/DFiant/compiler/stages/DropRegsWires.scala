package DFiant.compiler.stages

import DFiant.compiler.analysis.*
import DFiant.compiler.ir.DFRef.TwoWay
import DFiant.compiler.ir.{*, given}
import DFiant.compiler.patching.*
import DFiant.internals.*

import scala.collection.mutable
import scala.reflect.classTag

/** This stage transforms a register-transfer (RT) design/domain into a valid event-driven (ED)
  * design/domain. For this purpose it does the following:
  *   a. Adds clock and reset ports and connects them across designs.
  *   a. Converts the design/domain `domainType` from RT to ED.
  *   a. Adds combinational always blocks and moves the core logic to it.
  *   a. Drops register and wire declarations in favor of regular variable declarations. There are
  *      two kinds of variable declartions: local and global. Local variable declarations will apear
  *      inside the combinational always block and assignments to these variables are always
  *      blocking. Global variables are declared outside the always blocks (within the scope of the
  *      design/domain) and assignments to them are always non-blocking.
  *   a. Adds a sequential always block according to the clock and reset parameters and adds the
  *      register next value assignments to it.
  */
case object DropRegsWires extends Stage:
  // TODO: need derived clock and reset configuration to be explicit
  def dependencies: List[Stage] = List(DropRegAliases, SimpleOrderMembers, ViaConnection)
  def nullifies: Set[Stage] = Set()

  enum VarKind:
    case Local, Global, GlobalWithLocal
  object VarKind:
    def unapply(dcl: DFVal.Dcl)(using MemberGetSet): Option[VarKind] =
      dcl.modifier match
        // A register does not have a local variable, as it relies on `reg.din`
        case DFVal.Modifier.REG => Some(VarKind.Global)
        case DFVal.Modifier.WIRE =>
          val isGlobal = dcl.getConnectionTo.nonEmpty | dcl.getConnectionsFrom.nonEmpty
          if (isGlobal)
            if (dcl.getReadDeps.size > 1) Some(VarKind.GlobalWithLocal)
            else Some(VarKind.Global)
          else Some(VarKind.Local)
        // Not a register or wire declaration? Nothing to do.
        case _ => None
  end VarKind
  object WhenGlobalRefs extends Patch.Replace.RefFilter:
    def apply(refs: Set[DFRefAny])(using MemberGetSet): Set[DFRefAny] =
      refs.flatMap {
        case r: DFRef.TwoWayAny =>
          r.originRef.get match
            case n: DFNet if n.isLateConnection => Some(r)
            case _                              => None
        case _ => None
      }
  final val WhenLocalRefs = !WhenGlobalRefs

  def transform(designDB: DB)(using MemberGetSet): DB =
    val patchList: List[(DFMember, Patch)] = designDB.ownerMemberList.flatMap {
      // for all domain owners that are also blocks (RTDesign, RTDomain)
      case (owner: (DFDomainOwner & DFBlock & DFMember.Named), members) =>
        owner.domainType match
          // only care about register-transfer domains.
          // those have wires and regs that we need to simplify.
          case domainType: DomainType.RT =>
            // all the declarations that need to be converted to VARs
            val dclVars = members.collect { case v @ VarKind(kind) => (v, kind) }
            // all the registers
            val regs = members.collect {
              case dcl: DFVal.Dcl if dcl.modifier == DFVal.Modifier.REG => dcl
            }
            // name and existence indicators for the clock and reset
            val hasClock = domainType.clkCfg != None
            val clkName = domainType.clkCfg match
              case ClkCfg.Explicit(name: String, _) => name
              case _                                => "clk"
            val hasReset = domainType.rstCfg != None
            val rstName = domainType.rstCfg match
              case RstCfg.Explicit(name: String, _, _) => name
              case _                                   => "rst"

            // adding clock and reset ports according to the domain configuration
            val clkRstPortsDsn = new MetaDesign():
              lazy val clk = DFBit <> IN setName clkName
              if (hasClock) clk // touch lazy clk to create
              lazy val rst = DFBit <> IN setName rstName
              if (hasReset) rst // touch lazy rst to create
            val addClkRstPatchOption =
              if (hasClock || hasReset)
                Some(owner -> Patch.Add(clkRstPortsDsn, Patch.Add.Config.InsideFirst))
              else None

            // changing the owner from RT domain to ED domain
            val updatedOwner = owner match
              case design: DFDesignBlock => design.copy(domainType = DomainType.ED())
              case domain: DomainBlock   => domain.copy(domainType = DomainType.ED())
            val ownerDomainPatch =
              owner -> Patch.Replace(updatedOwner, Patch.Replace.Config.FullReplacement)

            val globalWithLocals = mutable.ListBuffer.empty[DFVal]
            val regVars = mutable.ListBuffer.empty[DFVal.Dcl]
            val globalsPatch = dclVars.flatMap {
              case (v, VarKind.Global) =>
                val rep = v.copy(modifier = DFVal.Modifier.VAR)
                if (v.modifier == DFVal.Modifier.REG) regVars += rep
                Some(v -> Patch.Replace(rep, Patch.Replace.Config.FullReplacement))
              case (v, VarKind.GlobalWithLocal) =>
                val rep = v.copy(modifier = DFVal.Modifier.VAR)
                globalWithLocals += rep
                Some(
                  v -> Patch.Replace(rep, Patch.Replace.Config.FullReplacement, WhenGlobalRefs)
                )
              case _ => None
            }
            var localsPatch = List.empty[(DFMember, Patch)]
            val localWithGlobals = mutable.ListBuffer.empty[DFVal]
            var regs_dinPatch = List.empty[(DFMember, Patch)]
            val alwaysBlockDsn = new MetaDesign():
              val regs_dinVars = regs.map { r =>
                r.asValAny.genNewVar(using dfc.setName(s"${r.name}_din")).asIR
              }

              always.all {
                localsPatch = dclVars.flatMap {
                  case (v, VarKind.Local) =>
                    val rep = v.asValAny.genNewVar(using dfc.setName(v.name)).asIR
                    Some(v -> Patch.Replace(rep, Patch.Replace.Config.ChangeRefAndRemove))
                  case (v, VarKind.GlobalWithLocal) =>
                    val rep = v.asValAny.genNewVar(using dfc.setName(s"${v.name}_v")).asIR
                    localWithGlobals += rep
                    Some(v -> Patch.Replace(rep, Patch.Replace.Config.ChangeRefOnly, WhenLocalRefs))
                  case _ => None
                }
                val reg_dinLocals = members
                  .collect { case reg_din: DFVal.Alias.RegDIN => reg_din }
                  .groupByOrdered(_.relValRef.get)
                  .view
                  .filter((_, reg_dins) => reg_dins.exists { rdi => rdi.getReadDeps.nonEmpty })
                  .map((r, _) =>
                    (r, r.asValAny.genNewVar(using dfc.setName(s"${r.name}_din_v")).asIR)
                  )
                  .toMap
                regs_dinPatch = regs.lazyZip(regs_dinVars).flatMap { (r, r_din_global) =>
                  val r_din_v = reg_dinLocals.get(r) match
                    case Some(local) =>
                      globalWithLocals += r_din_global
                      localWithGlobals += local
                      local
                    case None => r_din_global

                  members.collect {
                    case reg_din: DFVal.Alias.RegDIN if reg_din.relValRef.get == r =>
                      reg_din -> Patch.Replace(r_din_v, Patch.Replace.Config.ChangeRefAndRemove)
                  }
                }
              }
              import DFiant.core.DFIf
              import DFiant.core.NoType
              import DFiant.core.{asTokenOf, DFTypeAny, DFOwnerAny}
              def regInitBlock() =
                regVars.foreach {
                  case r if r.externalInit.nonEmpty =>
                    r.asVarAny := DFiant.core.DFVal.Const(
                      r.externalInit.get.head.asTokenOf[DFTypeAny]
                    )
                  case _ =>
                }
              def regSaveBlock() =
                regVars.lazyZip(regs_dinVars).foreach { (r, r_din_v) =>
                  r.asVarAny := r_din_v.asValAny
                }
              def ifRstActive =
                import clkRstPortsDsn.rst
                val RstCfg.Explicit(_, _, active: RstCfg.Active) = domainType.rstCfg
                val cond = active match
                  case RstCfg.Active.High => rst == 1
                  case RstCfg.Active.Low  => rst == 0
                DFIf.singleBranch(Some(cond), DFIf.Header(NoType), regInitBlock)
              def ifRstActiveElseRegSaveBlock(): Unit =
                val (_, rstBranch) = ifRstActive
                DFIf.singleBranch(None, rstBranch, regSaveBlock)
              def ifClkEdge(ifRstOption: Option[DFOwnerAny], block: () => Unit = regSaveBlock) =
                import clkRstPortsDsn.clk
                val ClkCfg.Explicit(_, edge: ClkCfg.Edge) = domainType.clkCfg
                val cond = edge match
                  case ClkCfg.Edge.Rising  => clk.rising
                  case ClkCfg.Edge.Falling => clk.falling
                DFIf.singleBranch(
                  Some(cond),
                  ifRstOption.getOrElse(DFIf.Header(NoType)),
                  block
                )

              if (hasClock && regs.nonEmpty)
                import clkRstPortsDsn.clk
                if (hasReset && regs.exists(_.externalInit.nonEmpty))
                  val RstCfg.Explicit(_, mode: RstCfg.Mode, _) = domainType.rstCfg
                  import clkRstPortsDsn.rst
                  mode match
                    case RstCfg.Mode.Sync =>
                      always(clk) {
                        ifClkEdge(None, ifRstActiveElseRegSaveBlock)
                      }
                    case RstCfg.Mode.Async =>
                      always(clk, rst) {
                        val (_, rstBranch) = ifRstActive
                        ifClkEdge(Some(rstBranch))
                      }
                else always(clk) { ifClkEdge(None) }
                end if
              end if

            val abOwnerIR = alwaysBlockDsn.getDB.members.collectFirst { case ab: AlwaysBlock =>
              ab
            }.get
            val alwaysBlockAllPatch =
              owner -> Patch.Add(alwaysBlockDsn, Patch.Add.Config.InsideLast)
            val alwaysBlockAllMembers = members.filter {
              case dcl: DFVal.Dcl                     => false
              case dsn: DFOwnerNamed                  => false
              case net: DFNet if net.isLateConnection => false
              case m                                  => true
            }
            val alwaysBlockMembersPatch =
              abOwnerIR -> Patch.Move(
                alwaysBlockAllMembers,
                Patch.Move.Config.InsideLast
              )
            val localToGlobalDsn = new MetaDesign():
              globalWithLocals.lazyZip(localWithGlobals).foreach { (g, l) =>
                g.asVarAny := l.asValAny
              }
            val localToGlobalPatch =
              abOwnerIR -> Patch.Add(localToGlobalDsn, Patch.Add.Config.InsideLast)
            List(
              Some(ownerDomainPatch),
              addClkRstPatchOption,
              Some(alwaysBlockAllPatch),
              Some(alwaysBlockMembersPatch),
              Some(localToGlobalPatch),
              globalsPatch,
              localsPatch,
              regs_dinPatch
            ).flatten
          // other domains
          case _ => None
      // other owners
      case _ => None
    }
    designDB.patch(patchList).sanityCheck
  end transform
end DropRegsWires

extension [T: HasDB](t: T) def dropRegsWires: DB = StageRunner.run(DropRegsWires)(t.db)
