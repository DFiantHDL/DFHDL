package DFiant.core
import DFiant.compiler.ir
import scala.reflect.ClassTag
import DFiant.internals.*

extension [M <: ir.DFMember](member: M)
  def addMember(using DFC): M =
    dfc.mutableDB.addMember(member)
  def replaceMemberWith(updated: M)(using DFC): M =
    dfc.mutableDB.replaceMember(member, updated)
  def tag[CT <: ir.DFTag: ClassTag](customTag: CT)(using dfc: DFC): M =
    import dfc.getSet
    member
      .setTags(_.tag(customTag))
      .setMeta(m =>
        if (m.isAnonymous && !dfc.getMeta.isAnonymous) dfc.getMeta else m
      )
  def removeTagOf[CT <: ir.DFTag: ClassTag](using dfc: DFC): M =
    import dfc.getSet
    member
      .setTags(_.removeTagOf[CT])
      .setMeta(m =>
        if (m.isAnonymous && !dfc.getMeta.isAnonymous) dfc.getMeta else m
      )
