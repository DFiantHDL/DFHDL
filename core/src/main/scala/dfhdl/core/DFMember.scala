package dfhdl.core
import dfhdl.compiler.ir
import scala.reflect.ClassTag
import dfhdl.internals.*

trait DFMember[+T <: ir.DFMember] extends Any:
  val irValue: T | DFError
  override def toString: String = irValue.toString

type DFMemberAny = DFMember[ir.DFMember]
object DFMember:
  extension [T <: ir.DFMember](member: DFMember[T])
    def asIR: T = member.irValue match
      case memberIR: T @unchecked => memberIR
      case err: DFError           => throw DFError.Derived(err)
  extension [M <: DFMemberAny](member: M)
    @metaContextDelegate
    def anonymize: M = ???

extension [M <: ir.DFMember](member: M)
  def addMember(using DFC): M =
    dfc.mutableDB.addMember(member)
  def replaceMemberWith(updated: M)(using DFC): M =
    dfc.mutableDB.replaceMember(member, updated)
  def removeTagOf[CT <: ir.DFTag: ClassTag](using dfc: DFC): M =
    import dfc.getSet
    member
      .setTags(_.removeTagOf[CT])
      .setMeta(m => if (m.isAnonymous && !dfc.getMeta.isAnonymous) dfc.getMeta else m)
end extension
