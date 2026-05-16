package dfhdl.compiler.stages

import dfhdl.compiler.analysis.*
import dfhdl.compiler.ir.*
import dfhdl.compiler.patching.*
import dfhdl.options.CompilerOptions
import scala.collection.mutable

/** This stage converts local parameters that are used in IOs to be design parameters with default
  * values, since VHDL does not support local parameters for IO access. These kind of design
  * parameters remain at their default (relative) values and are never directly applied.
  */
case object LocalToDesignParams extends Stage:
  override def runCondition(using co: CompilerOptions): Boolean =
    co.backend match
      case be: dfhdl.backends.vhdl => true
      case _                       => false
  override def dependencies: List[Stage] = List(
    GlobalizePortVectorParams,
    SimpleOrderMembers
  )
  override def nullifies: Set[Stage] = Set()
  def transform(designDB: DB)(using getSet: MemberGetSet, co: CompilerOptions): DB =
    given RefGen = RefGen.fromGetSet
    // adding design parameters with initial values set as the anonymized local parameters
    val patchList: List[(DFMember, Patch)] =
      designDB.designMemberList.flatMap { (design, members) =>
        val localConsts = members.view.collect {
          case const @ DclConst() => const.getName -> const
        }.toMap
        val designInstances = members.collect { case di: DFDesignInst => di.getDesignBlock }
        val exploredDesigns = if (design.isTopTop) design :: designInstances else designInstances
        exploredDesigns.flatMap { designInstance =>
          val ioLocalParams = designInstance.getIOLocalParams
          ioLocalParams.view.collect { lp =>
            val dsn = new MetaDesign(
              lp,
              Patch.Add.Config.ReplaceWithLast(Patch.Replace.Config.FullReplacement)
            ):
              val lpAnon = plantMember(lp.anonymize).asValAny
              val paramValue =
                if (designInstance.isTopTop) lpAnon
                else
                  val paramName = s"${designInstance.getName}_${lp.getName}"
                  localConsts.get(paramName).getOrElse(lpAnon.asIR).asValAny
              val dp = dfhdl.core.DFVal.DesignParam(paramValue, Some(lpAnon))(using
                dfc.setMeta(lp.meta)
              )
            dsn.patch
          }.toList
        }
      }
    designDB.patch(patchList)
  end transform
end LocalToDesignParams

extension [T: HasDB](t: T)
  def localToDesignParams(using co: CompilerOptions): DB =
    StageRunner.run(LocalToDesignParams)(t.db)
