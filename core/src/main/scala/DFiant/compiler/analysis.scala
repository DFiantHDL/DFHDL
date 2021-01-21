package DFiant
package compiler

import scala.annotation.tailrec
import internals._

object analysis {
  final implicit class DFAnyAnalysis(value: DFAny.Member)(implicit
      getSet: MemberGetSet
  ) {
    @tailrec def dealias: DFAny.Member = {
      value match {
        case alias: DFAny.Alias => alias.relValRef.get.dealias
        case v: DFAny.Member    => v
      }
    }
    //true if and only it is assigned at any of its dealiasing stages
    @tailrec def isNonAliasAssigned: Boolean = {
      value match {
        case alias: DFAny.Alias =>
          if (getSet.designDB.getAssignmentsTo(alias).nonEmpty) true
          else alias.relValRef.get.isNonAliasAssigned
        case v: DFAny.Member => getSet.designDB.getAssignmentsTo(v).nonEmpty
      }
    }
    private def partName(member: DFAny.Member) = s"${member.name}_part"
    @tailrec private def suggestName(
        member: DFAny.Member,
        prevMember: Option[DFAny.Member] = None
    ): String = {
      val refs = getSet.designDB.memberTable(member).map(r => (r, r.refType))
      val refOwner: Option[DFMember] = refs
      //search consuming references first
        .collectFirst {
          case (r: DFMember.OwnedRef, _: DFAny.Ref.ConsumeFrom.Type) =>
            r.owner.get
        }
        //search aliasing references, as long as we don't go back to previous member
        //(aliasing can be used for both producing and consuming)
        .orElse {
          refs.collectFirst {
            case (r: DFMember.OwnedRef, _: DFAny.Alias.RelValRef.Type)
                if prevMember.isEmpty || prevMember.get != r.owner.get =>
              r.owner.get
          }
        }
      refOwner match {
        //name from assignment or connection
        case Some(net: DFNet) => partName(net.toRef.get)
        //name from a named value which was referenced by an alias
        case Some(value: DFAny.Member) if !value.isAnonymous => partName(value)
        //found an (anonymous) value -> checking suggestion for it
        case Some(value: DFAny.Member) => suggestName(value, Some(value))
        //no named source found -> relying on the default anonymous naming
        case _ => member.name
      }
    }
    def suggestName: String = suggestName(value)
    def getNameOrSuggestion(implicit getSet: MemberGetSet): String =
      if (value.isAnonymous) value.suggestName else value.name
    def requiresDefaultInit: Boolean = {
      val refs = getSet.designDB.memberTable.getOrElse(value, Set())
      refs.exists {
        case r: DFMember.OwnedRef =>
          r.refType match {
            case _: DFAny.Alias.RelValRef.Type =>
              r.owner.get match {
                case _: DFAny.Alias.Prev => true
                case _                   => false
              }
            case _ => false
          }
        case _ => false
      }
    }
  }

  final implicit class DFMemberAnalysis(member: DFMember)(implicit
      getSet: MemberGetSet
  ) {
    @tailrec private def getCBOwners(
        currentOwner: DFBlock,
        list: List[DFConditional.Block]
    ): List[DFConditional.Block] =
      currentOwner match {
        case _: DFDesign.Block => list
        case cb: DFConditional.Block =>
          getCBOwners(currentOwner.getOwnerBlock, cb :: list)
      }

    //Gets the conditional blocks that own this member.
    //The highest owner is first in the list.
    def getCBOwners(includeCBSelf: Boolean = false): List[DFConditional.Block] =
      member match {
        case _: DFDesign.Block.Top                    => List()
        case cb: DFConditional.Block if includeCBSelf => List(cb)
        case _                                        => getCBOwners(member.getOwnerBlock, List())
      }
  }

  final implicit class IfBlockAnalysis(ifBlock: DFConditional.IfElseBlock)(
      implicit getSet: MemberGetSet
  ) {
    import DFConditional.IfElseBlock
    def getBranches: Iterable[IfElseBlock] = {
      val firstIfBlock = ifBlock.getFirstCB.asInstanceOf[IfElseBlock]
      firstIfBlock ::
        getSet.designDB
          .blockMemberTable(firstIfBlock.getOwnerBlock)
          .dropWhile(_ != firstIfBlock)
          .drop(1)
          .takeWhile {
            case ib: IfElseBlock if ib.prevBlockRefOption.isDefined => true
            case _                                                  => false
          }
          .collect {
            case ib: IfElseBlock => ib
          }
    }
  }
  final implicit class MatchHeaderAnalysis(mh: DFConditional.MatchHeader)(
      implicit getSet: MemberGetSet
  ) {
    import DFConditional.CaseBlock
    def getCases: Iterable[CaseBlock] = {
      getSet.designDB
        .blockMemberTable(mh.getOwnerBlock)
        .dropWhile(_ != mh)
        .drop(1)
        .takeWhile {
          case _: CaseBlock => true
          case _            => false
        }
        .collect {
          case cb: CaseBlock => cb
        }
    }

    @tailrec def caseOwnerOf(member: DFMember): Option[CaseBlock] =
      member match {
        case cb: CaseBlock if cb.matchHeaderRef.get == mh => Some(cb)
        case _: DFDesign.Block                            => None
        case _                                            => caseOwnerOf(member.getOwnerBlock)
      }
    def anyCaseContains(member: DFMember): Boolean =
      caseOwnerOf(member).isDefined
    //gets all the assignments within the match statement (at any level)
    def getAssignments: Iterable[DFNet] = {
      getSet.designDB.members.toIterable
        .dropWhile(_ != mh)
        .drop(1)                    //reaching the match header
        .takeWhile(anyCaseContains) //taking all case members
        .collect {
          case n: DFNet if n.isAssignment => n
        } //collecting assigned values
    }
    //gets all the assigned variables within the match statement (at any level), but defined externally
    def getExternalAssignedVars: Iterable[DFAny.Member] = {
      val assignments = getAssignments
      val casesNum    = getCases.size
      assignments
        .map(a => (a.toRef.get, caseOwnerOf(a).get)) //map to (toVar, caseOwner)
        .filterNot(x =>
          anyCaseContains(x._1)
        )                   //filtering out variables defined inside the cases
        .groupBy(x => x._1) //group with toVar
        .map(x =>
          (x._1, x._2.map(_._2).toSet)
        ) //map to (toVar -> Set[CaseBlock))
        .filter(x =>
          x._2.size == casesNum
        ) //keeping only variables assigned in all cases (fully covered)
        .keys
    }
  }

  final implicit class ConditionalBlockAnalysis(cb: DFConditional.Block)(
      implicit getSet: MemberGetSet
  ) {
    @tailrec def getFirstCB: DFConditional.Block =
      cb.prevBlockRefOption match {
        case Some(value) => value.get.getFirstCB
        case None        => cb
      }
    def isFirstCB: Boolean =
      cb.prevBlockRefOption match {
        case None => true
        case _    => false
      }
    def getNextCB: Option[DFConditional.Block] = {
      val refs = getSet.designDB.memberTable.getOrElse(cb, Set())
      //the conditional block is last if there is no reference to it as a previous block
      refs
        .collectFirst {
          case r @ DFConditional.PrevBlockRefOption() => r.owner.get
        }
        .collectFirst {
          case cb: DFConditional.Block => cb
        }
    }
    def isLastCB: Boolean = getNextCB.isEmpty
    @tailrec private def getPatterns(
        casePattenBlock: DFConditional.CaseBlock,
        patterns: List[DFAny.Pattern]
    ): List[DFAny.Pattern] = {
      val updatedPattens = casePattenBlock.patternOption.toList ++ patterns
      casePattenBlock.prevBlockRefOption match {
        case Some(r) => getPatterns(r.get, updatedPattens)
        case None    => updatedPattens
      }
    }
    @tailrec private def getLeadingCBChain(
        cb: DFConditional.Block,
        chain: List[DFConditional.Block]
    ): List[DFConditional.Block] =
      cb.prevBlockRefOption match {
        case Some(prevBlockRef) =>
          val prevBlock = prevBlockRef.get
          getLeadingCBChain(prevBlock, prevBlock :: chain)
        case None => chain
      }
    def getLeadingChain : List[DFConditional.Block] = getLeadingCBChain(cb, List(cb))
    def isExhaustive : Boolean = cb match {
      case DFConditional.IfElseBlock(None,_,_,_) => true //elsedf block
      case DFConditional.CaseBlock(_,_,None,_,_) => true //casedf_ block
      case x : DFConditional.CaseBlock if x.isLastCB =>
        val matchVal = x.matchHeaderRef.matchValRef.get
        val patterns = getPatterns(x, List())
        matchVal.dfType match {
          case dec : DFDecimal.Type[_,_,_] if !dec.signed.getValue && dec.fractionWidth.getValue == 0 =>
            val union = patterns.asInstanceOf[List[DFDecimal.Pattern]].foldLeft(IntervalSet.empty[BigInt]){case (is, p) => is | p.patternSet}
            val fullRange = Interval.closed(BigInt(0), BigInt.maxUnsignedFromWidth(matchVal.width))
            union.contains(fullRange)
          case dec : DFDecimal.Type[_,_,_] if dec.signed.getValue && dec.fractionWidth.getValue == 0 =>
            val union = patterns.asInstanceOf[List[DFDecimal.Pattern]].foldLeft(IntervalSet.empty[BigInt]){case (is, p) => is | p.patternSet}
            val fullRange = Interval.closed(BigInt.minSignedFromWidth(matchVal.width), BigInt.maxSignedFromWidth(matchVal.width))
            union.contains(fullRange)
          case _ : DFBits.Type[_] =>
            val union = patterns.asInstanceOf[List[DFBits.Pattern]].foldLeft(Set.empty[DFBits.Token]){case (s, p) => s | p.patternSet}
            union.size == BigInt.maxUnsignedFromWidth(matchVal.width).toInt + 1
          case _ : DFBool.Type =>
            val union = patterns.asInstanceOf[List[DFBool.Pattern]].foldLeft(Set.empty[Boolean]){case (s, p) => s | p.patternSet}
            union.size == 2
          case e : DFEnum.Type[_] =>
            val union = patterns.asInstanceOf[List[DFEnum.Pattern]].foldLeft(Set.empty[DFEnum.Entries.Entry]){case (s, p) => s | p.patternSet}
            union.size == e.entries.all.size
        }
      case _ => false
    }
    //Gets the topmost member of an if/match chain.
    //For ifs it's the if owner, and for matches it's the match header.
    @tailrec private def getTopConditionalMember(
        currentBlock: DFConditional.Block
    ): DFMember =
      currentBlock.getOwnerBlock match {
        case cb: DFConditional.Block => getTopConditionalMember(cb)
        case _ =>
          currentBlock match {
            case DFConditional.IfElseBlock(_, Some(prevBlockRef), _, _) =>
              prevBlockRef.get
            case block: DFConditional.IfElseBlock => block
            case block: DFConditional.CaseBlock   => block.matchHeaderRef.get
          }
      }
    def getTopConditionalMember: DFMember = getTopConditionalMember(cb)
  }

}
