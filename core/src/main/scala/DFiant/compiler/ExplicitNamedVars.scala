package DFiant
package compiler

import DFDesign.DB.Patch

import scala.annotation.tailrec
import DFAny.Modifier

final class ExplicitNamedVars[D <: DFDesign](c : IRCompilation[D]) {
  private val designDB = c.db
  import designDB.__getset

  //Gets the topmost member of an if/match chain.
  //For ifs it's the if owner, and for matches it's the match header.
  @tailrec private def getTopConditionalMember(currentBlock : ConditionalBlock.Owner) : DFMember =
    currentBlock.getOwnerBlock match {
      case cb : ConditionalBlock.Owner => getTopConditionalMember(cb)
      case _ => currentBlock match {
        case ConditionalBlock.IfElseBlock(_,Some(prevBlockRef),_,_) => prevBlockRef.get
        case block : ConditionalBlock.IfElseBlock => block
        case block : ConditionalBlock.CaseBlock => block.matchHeaderRef.get
      }
    }

  private def requiresDefaultInit(member : DFMember) : Boolean = {
    val refs = designDB.memberTable.getOrElse(member , Set())
    refs.exists{
      case r : DFMember.OwnedRef => r.refType match {
        case _ : DFAny.Alias.RelValRef.Type => r.owner.get match {
          case _ : DFAny.Alias.Prev => true
          case _ => false
        }
        case _ => false
      }
      case _ => false
    }
  }
  def explicitNamedVars : IRCompilation[D] = {
    val patchList = designDB.members.flatMap {
      case named : DFAny.Member if !named.isAnonymous => named match { //all named values
        case DFAny.Dcl(_,Modifier.MatchRetVar | Modifier.IfRetVar | Modifier.Port(_),_,_,_) => None //ignoring ports and match/if return variables
        case _ =>
          val anon = named.anonymize
          val externalInit = named.getInit match {
            case Some(i +: _) if !i.isBubble => Some(Seq(i))
            case _ => None
          }
          def newVar(implicit ctx : DFAny.Context) = DFAny.Dcl(named.dfType, DFAny.Modifier.NewVar, externalInit, ctx.owner, named.tags)
          named.getOwner match {
            case cb : ConditionalBlock.Owner => //inside a conditional block
              val dsnNewVar = new MetaDesign() {
                final val plantedNewVar = plantMember(newVar)
                if (externalInit.isDefined && requiresDefaultInit(named)) {
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
    c.newStage(designDB.patch(patchList))
  }
}
