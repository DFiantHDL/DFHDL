package dfhdl.compiler.patching
import dfhdl.compiler.ir.*

import scala.reflect.{ClassTag, classTag}

extension [T <: DFMember](member: T)
  def setName(name: String)(using MemberGetSet): T =
    getSet.set(member)(_.setMeta(_.setName(name)))
  def anonymize(using MemberGetSet): T =
    getSet.set(member)(_.setMeta(_.anonymize))
  def removeTagOf[CT <: DFTag: ClassTag](using MemberGetSet): T =
    getSet.set(member)(_.setTags(_.removeTagOf[CT]))
  def tag[CT <: DFTag: ClassTag](customTag: CT)(using MemberGetSet): T =
    getSet.set(member)(_.setTags(_.tag(customTag)))
