package DFiant
package compiler

import DFDesign.DB.Patch

import scala.annotation.tailrec
import DFAny.Modifier

final class ExplicitNamedVarsOps[D <: DFDesign, S <: shapeless.HList](c : Compilable[D, S]) {
  private val designDB = c.db
  import designDB.__getset

  //Gets the topmost member of an if/match chain.
  //For ifs it's the if owner, and for matches it's the match header.
  @tailrec private def getTopConditionalMember(currentBlock : ConditionalBlock) : DFMember =
    currentBlock.getOwnerBlock match {
      case cb : ConditionalBlock => getTopConditionalMember(cb)
      case _ => currentBlock match {
        case block: ConditionalBlock.IfBlock => block
        case block: ConditionalBlock.ElseIfBlock => getTopConditionalMember(block.prevBlockRef)
        case block: ConditionalBlock.ElseBlock => getTopConditionalMember(block.prevBlockRef)
        case block: ConditionalBlock.CasePatternBlock[_] => block.matchHeaderRef
        case block: ConditionalBlock.Case_Block[_] => block.matchHeaderRef
      }
    }
  def explicitNamedVars = {
    val patchList = designDB.members.flatMap {
      case named : DFAny if !named.isAnonymous => named match { //all named values
        case DFAny.Dcl(_,Modifier.MatchRetVar | Modifier.IfRetVar | Modifier.Port(_),_,_,_) => None //ignoring ports and match/if return variables
        case _ =>
          val anon = named.anonymize
          val externalInit = named.tags.init match {
            case Some(i +: _) if !i.isBubble => Some(Seq(i))
            case _ => None
          }
          val newVar = DFAny.Dcl(named.dfType, DFAny.Modifier.NewVar, externalInit, named.ownerRef, named.tags)
          named.getOwner match {
            case cb : ConditionalBlock => //inside a conditional block
              val dsnNewVar = new MetaDesign() {
                final val plantedNewVar = plantMember(newVar)
                if (externalInit.isDefined) {
                  plantedNewVar.assign(DFAny.Const.forced(plantedNewVar.dfType, externalInit.get.head))
                }
              }
              val anonAssign = named match {
                case DFAny.NewVar() => Nil
                case _ =>
                  val dsnAssign = new MetaDesign() {
                    dsnNewVar.plantedNewVar.assign(plantMember(anon))
                  }
                  List(named -> Patch.Add(dsnAssign, Patch.Add.Config.After))
              }
              val topConditionalMember = getTopConditionalMember(cb)
              anonAssign ++ List(
                topConditionalMember -> Patch.Add(dsnNewVar, Patch.Add.Config.Before),
                named -> Patch.Replace(dsnNewVar.plantedNewVar, Patch.Replace.Config.ChangeRefAndRemove)
              )
            case _ => named match { //inside a design block
              case DFAny.NewVar() => None
              case _ =>
                val dsn = new MetaDesign() {
                  val plantedNewVar = plantMember(newVar)
                  plantedNewVar.assign(plantMember(anon))
                }
                List(named -> Patch.Add(dsn, Patch.Add.Config.ReplaceWithFirst()))
            }
          }
      }
      case _ => None
    }
    c.newStage[ExplicitNamedVars](designDB.patch(patchList), Seq())
  }
}

trait ExplicitNamedVars extends Compilable.Stage