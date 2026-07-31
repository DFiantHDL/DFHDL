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
    inline def asIR: T =
      if (isNullRef(member)) uninitializedRefError("value")
      else
        (member.irValue: Any).runtimeChecked match
          case memberIR: T @unchecked => memberIR
          case err: DFError           => throw DFError.Derived(err)
          // only reachable when the member is a boxed value class holding a `null` IR value
          case _ => uninitializedRefError("value")
end DFMember

private[core] inline def isNullRef(value: Any): Boolean = value.asInstanceOf[AnyRef] eq null

// A `null` DFHDL entity can only come from a forward reference: Scala permits referencing a
// class member before its definition inside the class body and silently yields `null` for it.
// Reporting it as a regular elaboration error attaches the position and hierarchy of the
// operation that consumed it, instead of leaking a raw NullPointerException to the user.
private[core] def uninitializedRefError(kind: String): Nothing =
  throw new IllegalArgumentException(
    s"""|Found a reference to an uninitialized DFHDL $kind.
        |This is caused by a forward reference: the $kind is declared later in the class body.
        |To Fix:
        |Move the declaration before its first use.""".stripMargin
  )

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
