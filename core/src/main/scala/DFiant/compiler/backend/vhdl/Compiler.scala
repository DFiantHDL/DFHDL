package DFiant
package compiler
package backend
package vhdl

import DFiant.compiler.analysis.MatchHeaderAnalysis
import DFiant.internals.BigIntExtrasCO
import DFiant.sim._

import scala.collection.mutable
import compiler.printer.formatter._
import RTL.Analysis

final class Compiler[D <: DFDesign](c : IRCompilation[D]) {
  private def isSyncMember(member : DFMember)(implicit printer : Printer) : Boolean = member match {
    case RTL.IfBlock(_,_) | RTL.ElseIfBlock(_,_) => true
    case _ => false
  }
  private def getProcessStatements(block : DFBlock, filterFunc : DFMember => Boolean = _ => true)(
    implicit printer : Printer
  ) : List[String] = {
    import printer.config._
    val (_, statements) = printer.getSet.designDB.blockMemberTable(block).filter(filterFunc).foldRight(("", List.empty[String])) {
      case (cb @ DFConditional.IfElseBlock(None,Some(_),_,_), (_, statements)) =>
        (If.Else(getProcessStatements(cb)), statements)
      case (cb @ DFConditional.IfElseBlock(Some(condRef),Some(_),_,_), (closing, statements)) =>
        (If.ElsIf(Value.ref(condRef.get), getProcessStatements(cb), closing), statements)
      case (cb @ DFConditional.IfElseBlock(Some(condRef),None,_,_), (closing, statements)) =>
        ("", If(Value.ref(condRef.get), getProcessStatements(cb), closing) :: statements)
      case (cb @ DFConditional.CaseBlock(_,_,None,_,_), (_, statements)) =>
        (Case.When(Case.Choice.Others(), getProcessStatements(cb)), statements)
      case (cb @ DFConditional.CaseBlock(_,_,Some(pattern),_,_), (whens, statements)) =>
        val when = Case.When(Case.Choice.Pattern(pattern), getProcessStatements(cb))
        (if (whens.isEmpty) when else s"$when\n$whens", statements)
      case (mh : DFConditional.MatchHeader, (whens, statements)) =>
        val matchValStr = mh.matchValRef.get match {
          case v @ DFUInt(_) => s"$FN to_integer(${Value.ref(v)})"
          case v @ DFSInt(_) => s"$FN to_integer(${Value.ref(v)})"
          case v => Value.ref(v)
        }
        ("", Case(matchValStr, whens, printer.getSet.designDB.caseWithDontCare(mh)) :: statements)
      case (Net.Internal(netStr), (closing, statements)) => (closing, netStr :: statements)
      case (Sim.Assert(assertStr), (closing, statements)) => (closing, assertStr :: statements)
      case (Sim.Finish(finishStr), (closing, statements)) => (closing, finishStr :: statements)
      case (_, nochange) => nochange
    }
    statements
  }

  private def matchToIfs(getSet : MemberGetSet)(
    implicit revision : Revision
  ) : Iterable[DFConditional.MatchHeader] = revision match {
    case Revision.V93 =>
      val matchHeaders = getSet.designDB.members.collect{case mh : DFConditional.MatchHeader => mh}
      matchHeaders.filter {mh =>
        mh.matchConfig match {
          case MatchConfig.NoOverlappingCases => getSet.designDB.caseWithDontCare(mh)
          case MatchConfig.AllowOverlappingCases => true
        }
      }
    case Revision.V2008 =>
      Iterable.empty
  }

  private def matchForceCover(getSet : MemberGetSet) : Iterable[DFConditional.MatchHeader] = {
    implicit val __getSet : MemberGetSet = getSet
    val matchHeaders = getSet.designDB.members.collect{case mh : DFConditional.MatchHeader => mh}
    matchHeaders.filter { mh =>
      mh.matchValRef.get match {
        //For enumeration only, we check the bits-width exhaustively coverage, since RTL enumeration
        //is more limited than the DFiant enumeration coverage check
        case DFEnum(entries) if BigInt.maxUnsignedFromWidth(entries.width) > entries.all.size => entries match {
          case auto: DFEnum.Auto[_] => auto.encoding match {
            //In VHDL, the default encoding is be set by the synthesizer. Since the synthesizer knows the coverage,
            //it will handle don't-care encoding automatically and we don't need to force them.
            case DFEnum.Encoding.Default => false
            //Other encodings are implemented as constants when compiled to VHDL, and the underlying
            //enumeration type is changed to std_logic_vector. In this case, a width coverage is required.
            case _ => true
          }
          //Manual encoding force width check coverage.
          case _: DFEnum.Manual[_] => true
        }
        //Bits comparison doesn't cover "XXX" checks, so we need to force others clause for it
        case v @ DFBits(width) =>
          val cases = mh.getCases
          val hasCase_ = cases.exists(c => c.patternOption.isEmpty)
          if (!hasCase_ && ((BigInt.maxUnsignedFromWidth(width) + 1) == cases.size)) true
          else false
        //Because of to_integer conversion of the match value, we must force an others clause, since
        //to_integer is always producing 32-bit integer
        case DFUInt(_) | DFSInt(_) => true
        //Non-enumeration exhaustively coverage is already handled in the ExplicitPrev stage
        case _ => false
      }
    }
  }

  def vhdlCompile[R <: Revision](implicit revision : R): BackendStage.Compilation[D, Backend[R]] = {
    val designDB =
      c.dropUnreferenced
       .fixAnonymous
       .flattenNames
       .flattenStruct
       .moveCBDesigns
//       .controlDesigns
       .orderMembers(OrderMembers.Order.LazyConnectionLast)
       .explicitPrev
       .forceOthersCaseCoverage(matchForceCover)
       .convertMatchToIf(matchToIfs)
       .carryMathConversion
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
            (Port(p.name, Port.Dir.In(), Type(p.dfType), Init(p)) :: ports, signals, variables)
          case (p @ DFAny.Port.Out(), (ports, signals, variables)) =>
            (Port(p.name, Port.Dir.Out(), Type(p.dfType), Init(p)) :: ports, signals, variables)
          case (s : DFAny.Member, (ports, signals, variables))
            if !s.isAnonymous && (
              designDB.getConnectionTo(s).isDefined ||
              s.tags.customTags.values.exists{
                case _ : RTL.Tag[_] => true
                case _ => false
              } ||
              designDB.getAssignmentsFrom(s).exists(x => x.isTaggedWith(RTL.Tag.Mod.Reg))) =>
            (ports, Signal(s.name, Type(s.dfType), Init(s)) :: signals, variables)
          case (c : DFAny.Const, (ports, signals, variables)) if !c.isAnonymous =>
            (ports, Constant(c.name, Type(c.dfType), Init(c)) :: signals, variables)
          case (v : DFAny.Member, (ports, signals, variables)) if !v.isAnonymous =>
            (ports, signals, Variable(v.name, Type(v.dfType), Init(v)) :: variables)
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
          case Revision.V93 => Process.Sensitivity(designDB.getSensitivityList(design))
          case Revision.V2008 => Process.Sensitivity.all
        }
        val asyncProcess = Process("async_proc", asyncSensitivityList, variables, asyncStatements)
        val syncStatements = getProcessStatements(design, isSyncMember)
        val syncSensitivityList = Process.Sensitivity(members.collect {
          case cb @ RTL.IfBlock(clkOrReset,_) if cb.getOwner == design => clkOrReset.name
          case RTL.ElseIfBlock(clk,_) => clk.name
        })
        val emits = members.collect {
          case Emitter(emitStr) => emitStr
        }.mkString("\n")
        val syncProcess = Process("sync_proc", syncSensitivityList, List(), syncStatements)
        val statements = componentInstances ++ List(asyncProcess, syncProcess, emits)
        val entriesDcls = designDB.getLocalEnumEntries(design).map(e =>
          s"${EnumEntriesDcl(e)}\n${EnumEntriesDcl.body(e)}"
        ).toList
        val arrTypeDcls = designDB.getLocalArrTypes(design).map(e =>
          s"${ArrayTypeDcl(e)}\n"
        ).toList
        val declarations = entriesDcls ++ arrTypeDcls ++ signals
        val architecture = Architecture(s"${entityName}_arch", entityName, declarations, statements)
        val file = File(s"${designDB.top.designType}_pkg", entity, architecture)
        Some(BackendStage.File(s"${design.designType}.vhdl", s"$file"))
      case _ => None
    }
    val packageFile = BackendStage.File(s"${PackageFile.Name()}.vhdl", PackageFile())
    BackendStage.Compilation[D, Backend[R]](c.dsn, designDB, packageFile :: files)
  }
}