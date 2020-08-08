package DFiant
package compiler
package backend
package vhdl

import DFiant.sim._
import scala.collection.mutable
import printer.formatter._
import RTL.Analysis

final class Compiler[D <: DFDesign](c : IRCompilation[D]) {
  private def isSyncMember(member : DFMember)(implicit printer : Printer) : Boolean = member match {
    case RTL.IfBlock(_,_) | RTL.ElseIfBlock(_,_) => true
    case _ => false
  }
  private def getProcessStatements(block : DFBlock, filterFunc : DFMember => Boolean = _ => true)(
    implicit printer : Printer
  ) : List[String] = {
    val (_, statements) = printer.getSet.designDB.blockMemberTable(block).filter(filterFunc).foldRight(("", List.empty[String])) {
      case (cb @ ConditionalBlock.IfElseBlock(None,Some(_),_,_), (_, statements)) =>
        (If.Else(getProcessStatements(cb)), statements)
      case (cb @ ConditionalBlock.IfElseBlock(Some(condRef),Some(_),_,_), (closing, statements)) =>
        (If.ElsIf(Value.ref(condRef.get), getProcessStatements(cb), closing), statements)
      case (cb @ ConditionalBlock.IfElseBlock(Some(condRef),None,_,_), (closing, statements)) =>
        ("", If(Value.ref(condRef.get), getProcessStatements(cb), closing) :: statements)
      case (cb @ ConditionalBlock.CaseBlock(_,_,None,_,_), (_, statements)) =>
        (Case.When(Case.Choice.Others(), getProcessStatements(cb)), statements)
      case (cb @ ConditionalBlock.CaseBlock(_,_,Some(pattern),_,_), (whens, statements)) =>
        val when = Case.When(Case.Choice.Pattern(pattern), getProcessStatements(cb))
        (if (whens.isEmpty) when else s"$when\n$whens", statements)
      case (mh : ConditionalBlock.MatchHeader, (whens, statements)) =>
        //TODO: handle matchval func @ vhdl93
        ("", Case(Value.ref(mh.matchValRef.get), whens, printer.getSet.designDB.caseWithDontCare(mh)) :: statements)
      case (Net.Internal(netStr), (closing, statements)) => (closing, netStr :: statements)
      case (Sim.Assert(assertStr), (closing, statements)) => (closing, assertStr :: statements)
      case (Sim.Finish(finishStr), (closing, statements)) => (closing, finishStr :: statements)
      case (_, nochange) => nochange
    }
    statements
  }

  private def matchToIfs(getSet : MemberGetSet)(
    implicit revision : Revision
  ) : Iterable[ConditionalBlock.MatchHeader] = revision match {
    case Revision.V93 =>
      val matchHeaders = getSet.designDB.members.collect{case mh : ConditionalBlock.MatchHeader => mh}
      matchHeaders.filter {mh =>
        mh.matchConfig match {
          case MatchConfig.NoOverlappingCases => getSet.designDB.caseWithDontCare(mh)
          case MatchConfig.AllowOverlappingCases => true
        }
      }
    case Revision.V2008 =>
      Iterable.empty
  }

  def vhdlCompile[R <: Revision](implicit revision : R): BackendStage.Compilation[D, Backend[R]] = {
    val designDB =
      c.fixAnonymous
       .flattenNames
       .explicitPrev
       .convertMatchToIf(matchToIfs)
       .explicitConversions
       .uniqueDesigns
       .uniqueNames(reservedKeywords, caseSensitive = false)
       .toRTLForm
       .viaPortConnection
       .orderMembers
       .db

    import designDB.__getset
    implicit val printer : Printer = new Printer {
      val getSet : MemberGetSet = __getset
      val config : Printer.Config = new Printer.Config(revision)
    }
    val designTypes = mutable.Set.empty[String]
    val files = designDB.designMemberList.flatMap {
      case (design : DFDesign.Block.Internal, _) if design.inlinedRep.nonEmpty => None
      case (design : DFDesign.Block, members) if !designTypes.contains(design.designType) =>
        designTypes += design.designType
        val (ports, signals, variables) = members.foldRight((List.empty[String],List.empty[String],List.empty[String])){
          case (p @ DFAny.Port.In(), (ports, signals, variables)) =>
            (Port(p.name, Port.Dir.In(), Type(p), Init(p)) :: ports, signals, variables)
          case (p @ DFAny.Port.Out(), (ports, signals, variables)) =>
            (Port(p.name, Port.Dir.Out(), Type(p), Init(p)) :: ports, signals, variables)
          case (s : DFAny, (ports, signals, variables))
            if !s.isAnonymous && (
              designDB.getConnectionTo(s).isDefined ||
              s.tags.customTags.values.exists{
                case _ : RTL.Tag[_] => true
                case _ => false
              } ||
              designDB.getAssignmentsFrom(s).exists(x => x.isTaggedWith(RTL.Tag.Mod.Reg))) =>
            (ports, Signal(s.name, Type(s), Init(s)) :: signals, variables)
          case (v : DFAny, (ports, signals, variables)) if !v.isAnonymous =>
            (ports, signals, Variable(v.name, Type(v), Init(v)) :: variables)
          case (_, psv) => psv
        }
        val entityName = design.designType
        val entity = Entity(entityName, ports)
        val componentInstances = members.collect {
          case x : DFDesign.Block.Internal if x.inlinedRep.isEmpty =>
            val connections = designDB.blockMemberTable(x).collect {
              case Net.External(netStr) => netStr
            }
            ComponentInstance(x.name, x.designType, connections)
        }
        val asyncStatements = getProcessStatements(design, m => !isSyncMember(m))
        val asyncSensitivityList : String = revision match {
          case Revision.V93 => Process.Sensitivity.List(designDB.getSensitivityList(design))
          case Revision.V2008 => Process.Sensitivity.All()
        }
        val asyncProcess = Process("async_proc", asyncSensitivityList, variables, asyncStatements)
        val syncStatements = getProcessStatements(design, isSyncMember)
        val syncSensitivityList = Process.Sensitivity.List(members.collect {
          case cb @ RTL.IfBlock(clkOrReset,_) if cb.getOwner == design => clkOrReset.name
          case RTL.ElseIfBlock(clk,_) => clk.name
        })
        val emits = members.collect {
          case Emitter(emitStr) => emitStr
        }.mkString("\n")
        val syncProcess = Process("sync_proc", syncSensitivityList, List(), syncStatements)
        val statements = componentInstances ++ List(asyncProcess, syncProcess, emits)
        val enumTypeDcls = designDB.getLocalEnumTypes(design).map(e => EnumTypeDcl(e)).toList
        val declarations = enumTypeDcls ++ signals
        val architecture = Architecture(s"${entityName}_arch", entityName, declarations, statements)
        val file = File(s"${designDB.top.designType}_pkg", entity, architecture)
        Some(BackendStage.File(s"${design.designType}.vhdl", s"$file"))
      case _ => None
    }
    val packageFile = BackendStage.File(s"${PackageFile.Name()}.vhdl", PackageFile())
    BackendStage.Compilation[D, Backend[R]](c.dsn, designDB, packageFile :: files)
  }
}