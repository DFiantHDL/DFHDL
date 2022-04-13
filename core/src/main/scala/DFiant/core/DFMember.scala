package DFiant.core
import DFiant.compiler.ir
import scala.reflect.ClassTag
import DFiant.internals.*

// TODO: return AnyVal workaround after https://github.com/lampepfl/dotty/issues/14340
trait DFMember[+T <: ir.DFMember]: // extends Any:
  val irValue: T | DFError
  override def toString: String = irValue.toString

type DFMemberAny = DFMember[ir.DFMember]
object DFMember:
  extension [T <: ir.DFMember](member: DFMember[T])
    def asIR: T = member.irValue match
      case memberIR: T @unchecked => memberIR
      case err: DFError =>
        println(err)
        throw DFError.Derived(err)
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
