package DFiant.compiler.patching
import DFiant.compiler.ir.*

import scala.reflect.{ClassTag, classTag}

extension [T <: DFMember](member: T)
  def setName(name: String)(using MemberGetSet): T =
    getSet.set(member)(_.setMeta(_.setName(name)))
  def anonymize(using MemberGetSet): T =
    getSet.set(member)(_.setMeta(_.anonymize))
  def removeTagOf[CT <: DFTag: ClassTag](using MemberGetSet): T =
    getSet.set(member)(_.setTags(_.removeTagOf[CT]))
