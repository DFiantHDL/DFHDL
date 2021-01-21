package DFiant
package compiler

import DFiant.DFAny.Alias.Prev
import DFiant.DFNet.Op
import DFiant.internals.SetOps
import DependencyContext.{AssignmentMap, ConsumeRef, DependencyMap, DynamicConsumeMap, DynamicProduceMap}

import scala.annotation.tailrec
import scala.collection.immutable
import analysis._
import sim.DFSimMember

import scala.collection.immutable.ListMap

final case class DependencyContext(
  dependencyMap : DependencyMap, assignmentMap : AssignmentMap,
  dynamicConsumeMap: DynamicConsumeMap, dynamicProduceMap: DynamicProduceMap
)(implicit val getSet: MemberGetSet) {
  implicit val __this : DependencyContext = this
//  private def consumeFrom(
//    fromVal : DFAny.Member, relWidth : Int, relBitLow : Int, byRef : Boolean
//  )(implicit currentBlock : DFBlock) : Source =
//    fromVal match {
//      case alias : DFAny.Alias =>
//        def relSrc = consumeFrom(alias.relValRef.get, relWidth, relBitLow, byRef)
//        alias match {
//          case DFAny.Alias.AsIs(dfType,_,_,_,_) => relSrc.as(dfType)
//          case DFAny.Alias.Prev(_,_,step,kind,_,_) =>
//            //a previous value alias is using by reference only
//            val src = consumeFrom(alias.relValRef.get, relWidth, relBitLow, byRef = true)
//            kind match {
//              case Prev.State => src.prev(step)
//              case Prev.Pipe => src.pipe(step)
//            }
//          case DFAny.Alias.BitsWL(dfType,_,rv,rw,rbl,_,_) =>
//            consumeFrom(rv.get, rw, relBitLow + rbl, byRef).as(dfType)
//          case DFAny.ApplySel(dfType,_,rvRef,idxRef,_,_) =>
//            //For simplification, consuming the entirety of selection index and array
//            val relVal = rvRef.get
//            val relSrc = consumeFrom(relVal, relVal.width, 0, byRef)
//            val idxVal = idxRef.get
//            val idxSrc = consumeFrom(idxVal, idxVal.width, 0, byRef)
//            Source.ApplySel(dfType, relSrc, idxSrc).bitsWL(relWidth, relBitLow)
//        }
//      case DFAny.Fork(_,rv,_,_) => consumeFrom(rv.get, relWidth, relBitLow, byRef = false)
//      case dcl : DFAny.Dcl if !dcl.isPortIn && !byRef =>
//        getLatestSource(dcl).bitsWL(relWidth, relBitLow).as(dcl.dfType)
//      case DFAny.Const(_,token,_,_) => Source.Const(token)
//      case dfVal : DFAny.Member => Source.Latest(dfVal).bitsWL(relWidth, relBitLow).as(dfVal.dfType)
//    }

  protected[DependencyContext] def consumeFrom(fromRef : ConsumeRef)(
    implicit currentBlock : DFBlock
  ) : DependencyContext = {
    val fromVal = fromRef.get
    val fromSrc = getLatestSource(fromVal)
    copy(dependencyMap = dependencyMap.updated(fromRef, fromSrc))
  }

  @tailrec private def assignTo(
    toVal : DFAny.Member, net : DFNet, relWidth : Int, relBitLow : Int,
    fromSrc : Source
  )(implicit currentBlock : DFBlock) : DependencyContext = {
    toVal match {
      case DFAny.Alias.AsIs(_,_,rv,_,_) => assignTo(rv.get, net, relWidth, relBitLow, fromSrc)
      case DFAny.Alias.BitsWL(_,_,rv,rw,rbl,_,_) => assignTo(rv.get, net, relWidth, rbl + relBitLow, fromSrc)
      case DFAny.ApplySel(_,_,rv,_,_,_) =>
        assignTo(rv.get, net, relWidth, relBitLow, fromSrc)
      case dcl : DFAny.Dcl =>
        val withInit = net.op match {
          case Op.Assignment => false
          case Op.Connection | Op.LazyConnection => true
        }
        val assignedSrc =
            getLatestSource(dcl) //getting the last assignment or an empty source
            .assignWL(fromSrc, relWidth, relBitLow, withInit) //modifying the related bits with the new source
        val blockVersions = assignmentMap.getOrElse(dcl, ListMap())
        val versions = blockVersions.getOrElse(currentBlock, Vector()) :+ (net, assignedSrc)
        copy(assignmentMap = assignmentMap.updated(dcl, blockVersions.updated(currentBlock, versions)))
    }
  }

  protected[DependencyContext] def assignWith(net : DFNet)(
    implicit currentBlock : DFBlock
  ) : DependencyContext = {
    val fromVal = net.fromRef.get
    val byRef = net.op match {
      case Op.LazyConnection => true
      case Op.Assignment | Op.Connection => false
    }
    val fromSrc = getLatestSource(fromVal)
    val toVal = net.toRef.get
    copy(dependencyMap = dependencyMap.updated(net.fromRef, fromSrc)).assignTo(toVal, net, toVal.width, 0, fromSrc)
  }
  protected[DependencyContext] def branchEntry(enterBlock : DFConditional.Block) : DependencyContext = this
  protected[DependencyContext] def branchExit(exitBlock : DFConditional.Block) : DependencyContext = {
    if (exitBlock.isLastCB) {
      val cbChain = exitBlock.getLeadingChain
      val cbSet = cbChain.toSet
      val updatedAssignmentMap = assignmentMap.foldLeft(assignmentMap) {
        case (assignmentMap, (dcl, blockVersions)) => exitBlock match {
          case ifExitBlock : DFConditional.IfElseBlock =>
            val branches = blockVersions.view.keys.collect{case cb : DFConditional.IfElseBlock => cb}
            if (cbSet.intersectsWith(branches)) {
              val branchVersions = cbChain.collect {
                case cb @ DFConditional.IfElseBlock(Some(condRef), _, _, _) =>
                  (condRef.getSource, blockVersions.getLatestVersionIn(cb))
              }
              val ownerBlock = exitBlock.getOwnerBlock
              val fallBackVersion =
                if (ifExitBlock.condRefOption.isEmpty) blockVersions.getLatestVersionIn(ifExitBlock)
                else blockVersions.getLatestVersionIn(ownerBlock)
              val headBranch = branches.head
              val src = Source.IfElse(dcl, headBranch, branchVersions, fallBackVersion, withInit = false)
              val ownerVersions = blockVersions.getOrElse(ownerBlock, Vector()) :+ (headBranch, src)
              assignmentMap.updated(dcl, blockVersions.updated(ownerBlock, ownerVersions))
            } else assignmentMap
          case caseExitBlock : DFConditional.CaseBlock =>
            val cases = blockVersions.view.keys.collect{case cb : DFConditional.CaseBlock => cb}
            if (cbSet.intersectsWith(cases)) {
              val caseVersions = cbChain.collect {
                case cb @ DFConditional.CaseBlock(_,_,Some(pattern),_,_) =>
                  (pattern, blockVersions.getLatestVersionIn(cb))
              }
              val ownerBlock = exitBlock.getOwnerBlock
              val fallBackVersion =
                if (caseExitBlock.patternOption.isEmpty) Some(blockVersions.getLatestVersionIn(caseExitBlock))
                else if (!caseExitBlock.isExhaustive) Some(blockVersions.getLatestVersionIn(ownerBlock))
                else None
              val headCase = cases.head
              val src = Source.Match(dcl, headCase, caseVersions, fallBackVersion, withInit = false)
              val ownerVersions = blockVersions.getOrElse(ownerBlock, Vector()) :+ (headCase, src)
              assignmentMap.updated(dcl, blockVersions.updated(ownerBlock, ownerVersions))
            } else assignmentMap
        }
      }
      copy(assignmentMap = updatedAssignmentMap)
    } else this
  }

  @tailrec def getLatestVersion(member : DFAny.Member)(implicit currentBlock : DFBlock) : SourceVersion = member match {
    case dcl : DFAny.Dcl if !dcl.isPortIn => assignmentMap.get(dcl) match {
      case Some(blockVersions) => blockVersions.getLatestVersionIn(currentBlock)
      case None => SourceVersion.Empty
    }
    case prev : DFAny.Alias.Prev => SourceVersion.Latest
    case alias : DFAny.Alias => getLatestVersion(alias.relValRef.get)
    case _ => SourceVersion.Latest
  }

  def getLatestSource(member : DFAny.Member, withInit : Boolean = true)(implicit currentBlock : DFBlock) : Source = {
    val version = getLatestVersion(member)
    Source(member, version, withInit)
  }


  def printAssignmentMap : DependencyContext = {
    assignmentMap.foreach {
      case (dcl, blockVersions) => blockVersions.foreach {
        case (block, versions) =>
          val isCB = block match {
            case block : DFDesign.Block => false
            case block : DFConditional.Block => true
          }
          if (versions.size > 1 || isCB)
            versions.zipWithIndex.foreach {
              case ((_, source), i) =>
                println(f"${dcl.getFullName}%-40s ${source.codeString}")
            }
          else println(f"${dcl.getFullName}%-40s ${versions.head._2.codeString}")
      }
    }
    this
  }

  def printDeps : DependencyContext = {
    dependencyMap.values.foreach(x => println(x.codeString))
    this
  }

  lazy val initMap : SourceValueEvaluation[Seq[DFAny.Token]] = getSet.designDB.evaluate(InitEvaluator)
  lazy val constMap : SourceValueEvaluation[DFAny.Token] = getSet.designDB.evaluate(ConstEvaluator)
//  lazy val stateVars : Set[DFAny.Dcl] = getSet.designDB.evaluate(CyclicEvaluator).view.collect {
//    case (SourceValue.Dcl(dcl, _), true) if dcl.isAssignable => dcl
//  }.toSet

  protected[DependencyContext] def dontProduce(toVal : DFAny.Member) : DependencyContext = ???
  protected[DependencyContext] def dontConsume(fromVal : DFAny.Member) : DependencyContext = ???
  protected[DependencyContext] def consume(fromVal : DFAny.Member) : DependencyContext = this
}

object DependencyContext {
  type DFAnyRef = DFAny.Ref[_ <: DFAny.Ref.Type]
  type ConsumeRef = DFAny.Ref[_ <: DFAny.Ref.ConsumeFrom.Type]
  type DependencyMap = ListMap[ConsumeRef, Source]
  type Versions = Vector[(DFMember, Source)]
  type BlockVersions = ListMap[DFBlock, Versions]
  type AssignmentMap = Map[DFAny.Dcl, BlockVersions]
  //Conditional Block Version Map
//  type CBVersionMap = Map[(DFAny.Dcl, DFConditional.Block), Int]
  type DynamicConsumeMap = Map[DFAny.Fork, Source]
  type DynamicProduceMap = Map[DFAny.Dcl, Source]

  def empty(implicit getSet: MemberGetSet) : DependencyContext =
    DependencyContext(ListMap(), Map(), Map(), Map())

  //retrieves a list of variables that are consumed as their implicit previous value.
  //the assignment stack map is pushed on every conditional block entry and popped on the block exit
  @tailrec private def getDependencies(
    remaining : List[DFMember],
    dependencyContext: DependencyContext
  )(implicit currentBlock : DFBlock) : DependencyContext = {
    import dependencyContext.getSet
    remaining match {
      case (nextBlock : DFBlock) :: rs if nextBlock.getOwnerBlock == currentBlock => //entering child block
        val updatedDependency : DependencyContext = nextBlock match {
          //for the if branch we get all the conditions and consume from them
          case cb : DFConditional.IfElseBlock if cb.isFirstCB =>
            val condRefs = cb.getBranches.collect {
              case DFConditional.IfElseBlock(Some(condRef),_,_,_) => condRef
            }
            condRefs.foldLeft(dependencyContext)(_.consumeFrom(_)).branchEntry(cb)
          case cb : DFConditional.Block =>
            dependencyContext.branchEntry(cb)
          case _ => dependencyContext
        }
        getDependencies(rs, updatedDependency)(nextBlock)
      case r :: rs if r.getOwnerBlock == currentBlock => //checking member consumers
        val updatedDependency : DependencyContext = r match {
          case net : DFNet => dependencyContext.assignWith(net)
          case applySel : DFAny.ApplySel =>
            dependencyContext.consumeFrom(applySel.idxRef)
          case func : DFAny.Func1 =>
            dependencyContext.consumeFrom(func.leftArgRef)
          case func : DFAny.Func2 =>
            dependencyContext.consumeFrom(func.leftArgRef).consumeFrom(func.rightArgRef)
          case assert : DFSimMember.Assert =>
            val condDep =
              assert.condOptionRef.foldLeft(dependencyContext)((d, r) => d.consumeFrom(r))
            assert.msgRef.seq
              .collect{case Left(x) => x} //get just the reference to dataflow values
              .foldLeft(condDep)((d, r) => d.consumeFrom(r))
          case matchHeader : DFConditional.MatchHeader =>
            dependencyContext.consumeFrom(matchHeader.matchValRef)
          case fork : DFAny.Fork =>
            dependencyContext.consumeFrom(fork.relValRef)
          case DFAny.Dynamic.DontProduce(relVal,_,_) => dependencyContext.dontProduce(relVal)
          case DFAny.Dynamic.DontConsume(relVal,_,_) => dependencyContext.dontConsume(relVal)
          case DFAny.Dynamic.Consume(relVal,_,_) => dependencyContext.consume(relVal)
          //          case outPort @ DFAny.Port.Out() =>
          //            (currentSet, scopeMap + (outPort -> AssignedScope.empty))
          //          case anyVar @ DFAny.NewVar() =>
          //            (currentSet, scopeMap + (anyVar -> AssignedScope.empty))
          case _ => dependencyContext
        }
        getDependencies(rs, updatedDependency)
      case _ => //exiting child block or no more members
        val updatedDependency = currentBlock match {
          case d : DFDesign.Block if remaining.isEmpty =>
            val outPorts : List[DFAny.Member] = getSet.designDB.designMemberTable(d).collect {
              case p @ DFAny.Port.Out() => p
              case p @ DFAny.NewVar() => p
            }
            outPorts.foldLeft(dependencyContext){case (d, p) => d.consume(p)}
          case _ =>
            dependencyContext
        }
        val exitingBlock = remaining match {
          case r :: _ if r.getOwnerBlock != currentBlock => true //another member but not a child of current
          case Nil if (currentBlock != getSet.designDB.top) => true //there are no more members, but still not at top
          case _ => false //no more members and we are currently back at top
        }
        if (exitingBlock) {
          val updatedDependency = currentBlock match {
            case cb : DFConditional.Block =>
              //                println(s"exiting $cb", cb.isLastCB, cb.isExhaustive)
              //                val ret =
              dependencyContext.branchExit(cb)
            //                println(s"${if (scopeMap.nonEmpty) scopeMap.head._2.toString else "<>"} => ${if (ret.nonEmpty) ret.head._2.toString else "<>"}")
            //                ret
            case _ => dependencyContext
          }
          getDependencies(remaining, updatedDependency)(currentBlock.getOwnerBlock)
        } else dependencyContext
    }
  }

  def apply(designDB : DFDesign.DB) : DependencyContext = {
    import designDB.__getset
    getDependencies(designDB.members.drop(1), DependencyContext.empty)(designDB.top)
  }
}
