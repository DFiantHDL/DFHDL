package DFiant.compiler.patching
import DFiant.compiler.ir.*

extension [T <: DFMember](member: T)
  def setName(name: String)(using MemberGetSet): T =
    getSet.set(member)(_.setMeta(_.setName(name)))
  def anonymize(using MemberGetSet): T =
    getSet.set(member)(_.setMeta(_.anonymize))
