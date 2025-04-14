package dfhdl.compiler.stages

import dfhdl.compiler.analysis.*
import dfhdl.compiler.ir.*
import dfhdl.compiler.patching.*
import dfhdl.internals.*
import dfhdl.options.CompilerOptions
import scala.collection.mutable
import dfhdl.core.DFOpaque as coreDFOpaque
import dfhdl.core.asFE

/** This stage adds clock and reset input ports across the entire design. This stage is run after
  * ExplicitClkRstCfg, so no Derived is expected here. The rules are:
  *   1. Explicit Clk/Rst configuration always causes Clk/Rst port inputs to be added, unless they
  *      are already explicitly declared by the user or an internal design has outputs of Clk/Rst of
  *      the same configuration.
  *   1. Related Clk/Rst configuration does not add Clk/Rst ports.
  *   1. Simulation top-level designs (do not have any ports) will have Clk/Rst VARs that are driven
  *      according to their configuration.
  */
case object AddClkRst extends Stage:
  def dependencies: List[Stage] = List(ToRT, ExplicitClkRstCfg)
  def nullifies: Set[Stage] = Set(ViaConnection)
  def transform(designDB: DB)(using MemberGetSet, CompilerOptions): DB =
    // saves domains that are outputting clk and rst
    val designDomainOut = mutable.Set.empty[(DFDesignBlock, RTDomainCfg)]
    // saves clk type used for a domain
    val clkTypeMap = mutable.Map.empty[RTDomainCfg.Explicit, coreDFOpaque[coreDFOpaque.Clk]]
    // saves rst type used for a domain
    val rstTypeMap = mutable.Map.empty[RTDomainCfg.Explicit, coreDFOpaque[coreDFOpaque.Rst]]
    // saves opaque clk/rst type replacements
    val opaqueReplaceMap = mutable.Map.empty[DFOpaque, DFOpaque]
    val patchList: List[(DFMember, Patch)] = designDB.domainOwnerMemberList.flatMap {
      // for all designs
      case (owner, members) =>
        val design = owner.getThisOrOwnerDesign
        // new configuration unapply object
        object NewCfg:
          // a configuration that was not yet handled by an internal design that has a domain output
          def unapply(cfg: RTDomainCfg.Explicit): Option[(ClkCfg, RstCfg)] =
            val RTDomainCfg.Explicit(_, clkCfg, rstCfg) = cfg
            if (!designDomainOut.contains((design, cfg)))
              Some(clkCfg, rstCfg)
            else None
        end NewCfg
        val ownerDomainPatchOption = owner.domainType match
          // just register-transfer domains with new configuration
          case DomainType.RT(cfg @ NewCfg(clkCfg, rstCfg)) =>
            // clk and rst names are according to the configuration
            val requiredClkName = clkCfg match
              case ClkCfg.Explicit(portName = portName) => Some(portName)
              case _                                    => None
            val requiredRstName = rstCfg match
              case RstCfg.Explicit(portName = portName) => Some(portName)
              case _                                    => None
            // collect existing clk and rst DFHDL value members
            val existingClk = members.collectFirst {
              case clk: DFVal.Dcl if clk.isClkDcl =>
                // if clk is an output then this is an output domain.
                // if the design is not a top-level, then the design owner is the receiver of this domain.
                if (clk.modifier.dir == DFVal.Modifier.OUT && !design.isTop)
                  designDomainOut += Tuple2(design.getOwnerDesign, cfg)
                clk
            }
            val existingRst = members.collectFirst {
              case rst: DFVal.Dcl if rst.isRstDcl => rst
            }
            // need to add clk and rst flags
            val addClk = requiredClkName.nonEmpty && existingClk.isEmpty
            val addRst = requiredRstName.nonEmpty && existingRst.isEmpty
            val magnetNameSuffix =
              // special-casing a default configuration name to be simply "default" for naming Clk/Rst types
              if (cfg.name.startsWith("RTDomainCfg.Default")) "default"
              // special-casing a config name with `<name>.norst` is derived from a configuration with a reset,
              // so the clocks are the same and the name mangling should be dropped.
              else cfg.name.replaceFirst(".norst", "")
            // clk and rst magnet types are either fetched from the memoization
            // or created and memoized
            val clkType =
              class Unique:
                case class Clk() extends coreDFOpaque.Clk:
                  override lazy val typeName: String = s"Clk_${magnetNameSuffix}"
              // the memoization for clock is always for the `.norst`, so only a single memoization
              // is required for both regular and `.norst` versions
              clkTypeMap.getOrElseUpdate(cfg.norst, coreDFOpaque(Unique().Clk()))
            val rstType =
              class Unique:
                case class Rst() extends coreDFOpaque.Rst:
                  override lazy val typeName: String = s"Rst_${magnetNameSuffix}"
              rstTypeMap.getOrElseUpdate(cfg, coreDFOpaque(Unique().Rst()))
            // saving changed opaques to change in all relevant members later
            existingClk.foreach(dcl =>
              opaqueReplaceMap += dcl.dfType.asInstanceOf[DFOpaque] -> clkType.asIR
            )
            existingRst.foreach(dcl =>
              opaqueReplaceMap += dcl.dfType.asInstanceOf[DFOpaque] -> rstType.asIR
            )
            // adding missing clk/rst ports patch
            if (addClk || addRst)
              val simGen = designDB.inSimulation && design.isTop
              val dsn = new MetaDesign(owner, Patch.Add.Config.InsideFirst):
                val selfDFC = dfc
                if (simGen)
                  val clkRstSimGen = new EDDomain:
                    override protected def __dfc: DFC =
                      selfDFC.setName("clkRstSimGen")
                        .setAnnotations(List(dfhdl.hw.flattenMode.transparent()))
                    val dfcAnon = selfDFC.anonymize.setAnnotations(Nil)
                    val clk = (clkType <> VAR)(using dfcAnon.setName(requiredClkName.get))
                    lazy val rst = (rstType <> VAR)(using dfcAnon.setName(requiredRstName.get))
                    if (addRst) rst // touch lazy rst to create
                    locally {
                      given DFC = selfDFC.anonymize.setAnnotations(Nil)
                      val clkRate =
                        clkCfg.asInstanceOf[ClkCfg.Explicit].rate.getConstData.get.asInstanceOf[
                          (BigDecimal, DFPhysical.Unit.Freq.Scale | DFPhysical.Unit.Time.Scale)
                        ]
                      val clkActive = clkCfg.asInstanceOf[ClkCfg.Explicit].edge match
                        case ClkCfg.Edge.Rising  => true
                        case ClkCfg.Edge.Falling => false
                      val clkPeriodHalf =
                        clkRate._2 match
                          case time: DFPhysical.Unit.Time.Scale =>
                            (clkRate._1 / 2, time)
                          case freq: DFPhysical.Unit.Freq.Scale =>
                            val (period, scale) = freq.to_period(clkRate._1)
                            (period / 2, scale)
                      lazy val rstActive = rstCfg.asInstanceOf[RstCfg.Explicit].active match
                        case RstCfg.Active.Low  => false
                        case RstCfg.Active.High => true
                      def clkPeriodHalfConst(using DFC): dfhdl.core.DFConstOf[dfhdl.core.DFTime] =
                        dfhdl.core.DFVal.Const(dfhdl.core.DFTime, clkPeriodHalf)
                      process.forever {
                        if (addRst)
                          rst.actual := dfhdl.core.DFVal.Const(dfhdl.core.DFBit, Some(rstActive))
                        val cond: Boolean <> VAL = true
                        dfhdl.core.DFWhile.plugin(cond) {
                          clk.actual := dfhdl.core.DFVal.Const(dfhdl.core.DFBit, Some(!clkActive))
                          wait(clkPeriodHalfConst)
                          clk.actual := dfhdl.core.DFVal.Const(dfhdl.core.DFBit, Some(clkActive))
                          wait(clkPeriodHalfConst)
                          if (addRst)
                            rst.actual := dfhdl.core.DFVal.Const(dfhdl.core.DFBit, Some(!rstActive))
                        }
                      }
                    }
                else
                  lazy val clk = (clkType <> IN)(using dfc.setName(requiredClkName.get))
                  if (addClk) clk // touch lazy clk to create
                  lazy val rst = (rstType <> IN)(using dfc.setName(requiredRstName.get))
                  if (addRst) rst // touch lazy rst to create
                end if
              // the ports are added as first members
              Some(dsn.patch)
            else None
            end if
          case _ => None
        // replace clk/rst value DFTypes with updated ones
        val opaqueTypeReplacePatches = members.view.flatMap {
          case dfVal: DFVal =>
            dfVal.dfType match
              case dfType @ DFOpaque(id = _: (DFOpaque.Clk | DFOpaque.Rst)) =>
                Some(
                  dfVal -> Patch.Replace(
                    dfVal.updateDFType(opaqueReplaceMap(dfType)),
                    Patch.Replace.Config.FullReplacement
                  )
                )
              case _ => None
          case _ => None
        }
        List(
          opaqueTypeReplacePatches,
          ownerDomainPatchOption
        ).flatten
    }
    designDB.patch(patchList)
  end transform
end AddClkRst

extension [T: HasDB](t: T)
  def addClkRst(using CompilerOptions): DB = StageRunner.run(AddClkRst)(t.db)
