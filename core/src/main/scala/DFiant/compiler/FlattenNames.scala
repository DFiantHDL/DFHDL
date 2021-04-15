package DFiant
package compiler

import DFDesign.DB.Patch

import scala.annotation.tailrec
import scala.reflect.{ClassTag, classTag}
import collection.mutable

final class FlattenNames[D <: DFDesign](c : IRCompilation[D]) {
  private val designDB = c.db
  import designDB.__getset
  @tailrec private def recursiveNameFlatten(member : DFMember, nameFunc : String => String) : String => String = member.getOwner match {
    case o : DFOwner.NameFlattenOwner =>
      def suggestOwnerName : Option[String] = o.getOwner.getMembers.collectFirst {
        case net : DFNet if net.tags.meta.namePosition == o.tags.meta.namePosition => s"${net.toRef.get.name}_part"
        case m : DFAny.Member if !m.isAnonymous && m.tags.meta.namePosition == o.tags.meta.namePosition => m.name
      }
      o.nameFlatten match {
        //explicitly requested to ignore the name of the owner
        case DFOwner.NameFlatten.IgnoreOwnerName => recursiveNameFlatten(o, nameFunc)
        //the owner has only a single named member, and therefore the member will get the name of the owner
        case _ if o.getMembers.view.filterNot(_.isAnonymous).size == 1 =>
          if (o.isAnonymous) recursiveNameFlatten(o, _ => suggestOwnerName.getOrElse(""))
          else recursiveNameFlatten(o, _ => o.name)
        //the owner is anonymous, so we ignore its name
        case _ if o.isAnonymous =>
          recursiveNameFlatten(o, n => suggestOwnerName.map(o.nameFlatten(nameFunc(n), _)).getOrElse(nameFunc(n)))
        //name everything else according to the name flattening function
        case _ => recursiveNameFlatten(o, n => o.nameFlatten(nameFunc(n), o.name))
      }
    case _ => nameFunc
  }
  private def recursiveNameFlatten(member : DFMember) : String => String = recursiveNameFlatten(member, n => n)
  def flattenNames : IRCompilation[D] = {
    val (patchList, tagList) = designDB.members.foldRight((List.empty[(DFMember, Patch)], List.empty[((Any, ClassTag[_]), DFMember.CustomTag)])) {
      //skip top
      case (_ : DFDesign.Block.Top, ret) => ret
      //owners are flattened by replacing their reference with their own owner, so all their current members
      //will reference their new owners (grandparents)
      case (o : DFOwner.NameFlattenOwner, ret) =>
        (o -> Patch.Replace(o.getOwnerBlock, Patch.Replace.Config.ChangeRefAndRemove) :: ret._1, ret._2)
      //for all named members do:
      case (m, ret) if !m.isAnonymous => m.getOwner match {
        //
        case o: DFOwner.NameFlattenOwner if o.tags.meta.namePosition == m.tags.meta.namePosition =>
          (m -> Patch.Replace(m.anonymize, Patch.Replace.Config.FullReplacement) :: ret._1, ret._2)
        case o: DFOwner.NameFlattenOwner =>
//          if (o.tags.meta.namePosition == m.tags.meta.namePosition) println(o.name)
          val updatedNameFunc = recursiveNameFlatten(m)
          val updatedName = updatedNameFunc(m.name)
          val updatedTags = m match {
            case DFAny.Dcl(DFEnum.Type(entries),DFAny.Modifier.NewVar,_,_,_) =>
              val tag = (entries, classTag[DFMember.NameTag]) -> DFMember.NameTag(updatedNameFunc(entries.name))
              tag :: ret._2
            case _ =>
              ret._2
          }
          val updatedMember = if (updatedName.isEmpty) m.anonymize else m.setName(updatedName)
          if (updatedMember == m) ret
          else (m -> Patch.Replace(updatedMember, Patch.Replace.Config.FullReplacement) :: ret._1, updatedTags)
        case _ => ret
      }
      case (_, ret) => ret
    }
    c.newStage(designDB.patch(patchList).setGlobalTags(tagList))
  }
  object DesignableScope {
    def unapply(owner : DFScope.Owner) : Boolean = {
      owner.getMembers.forall {
        case net : DFNet => net.toRef.get.isInsideOwner(owner)
        case DFAny.Dynamic.DontProduce(m,_,_) => m.isInsideOwner(owner)
        case _ : DFScope.Owner => false
        case _ => true
      }
    }
  }
  protected def protScopesToDesigns : (Boolean, IRCompilation[D]) = {
    val replacementTable = mutable.Map.empty[DFAny.Member, DFAny.Member]
    val patchList = designDB.members.flatMap {
      case scope @ DesignableScope() =>
        val members = scope.getMembers
        val inValues = members.view.flatMap {
          case net : DFNet => Some(net.fromRef.get)
          case alias : DFAny.Alias => Some(alias.relValRef.get)
          case apply : DFAny.ApplySel => List(apply.relValRef.get, apply.idxRef.get)
          case func : DFAny.Func1 => Some(func.leftArgRef.get)
          case func : DFAny.Func2 => List(func.leftArgRef.get, func.rightArgRef.get)
          case fork : DFAny.Fork => Some(fork.relValRef.get)
          case dynamic : DFAny.Dynamic => Some(dynamic.relValRef.get)
          case _ => None
        }.filter(_.isOutsideOwner(scope)).toList.distinct
        val outValues = members.collect {
          case v : DFAny.Member if
            designDB.memberTable.getOrElse(v, Set()).view
            .exists{
              case r : DFMember.OwnedRef => r.owner.get.isOutsideOwner(scope)
              case _ => false
            } => v
        }
        val portDsn = new MetaDesign() {
          val newOwner = DFDesign.Block.Internal(this)(scope.defMeta.get.name,None,sim.DFSimDesign.Mode.Off).setTags(_ => scope.tags).asInstanceOf[DFOwner]
          __ctx.db.OwnershipContext.injectOwner(newOwner)
          val i = inValues.zipWithIndex.map{case (v, i) => DFAny.Dcl(v.dfType, DFAny.Modifier.Port(IN), None).setName(s"i$i")}
          val o = outValues.zipWithIndex.map{case (v, i) => DFAny.Dcl(v.dfType, DFAny.Modifier.Port(OUT), None).setName(s"o$i")}
          __ctx.db.OwnershipContext.injectOwner(owner)
        }
        val outConnDsn = new MetaDesign() {
          val net = outValues.lazyZip(portDsn.o).map {(ov, op) =>
            replacementTable += (ov -> op)
            DFNet.Connection(op, ov)
          }
        }
        val inConnDsn = new MetaDesign() {
          val net = inValues.lazyZip(portDsn.i).map {(iv, ip) =>
            DFNet.Connection(ip, replacementTable.getOrElse(iv, iv))
          }
        }
        val inPatchList = inValues.lazyZip(portDsn.i).map { (iv, ip) =>
          iv -> Patch.Replace(ip, Patch.Replace.Config.ChangeRefOnly, Patch.Replace.RefFilter.Inside(scope))
        }
        val outPatchList = outValues.lazyZip(portDsn.o).map {(ov, op) =>
          ov -> Patch.Replace(op, Patch.Replace.Config.ChangeRefOnly, Patch.Replace.RefFilter.Outside(scope))
        }
        List(
          Some(scope -> Patch.Add(portDsn, Patch.Add.Config.ReplaceWithFirst())),
          Some(scope -> Patch.Add(outConnDsn, Patch.Add.Config.InsideLast)),
          Some(scope -> Patch.Add(inConnDsn, Patch.Add.Config.After)),
          inPatchList,
          outPatchList,
        ).flatten
      case _ => None
    }
    if (patchList.isEmpty) (false, c)
    else (true, c.newStage(designDB.patch(patchList, debug = true)))
  }
  def scopesToDesigns : IRCompilation[D] = {
    var run = c.protScopesToDesigns
    while (run._1) run = run._2.protScopesToDesigns
    run._2
  }
}