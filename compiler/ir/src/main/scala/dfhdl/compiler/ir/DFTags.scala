package dfhdl.compiler.ir
import scala.reflect.{ClassTag, classTag}

sealed trait DFTag extends Product with Serializable
sealed trait DFTagOf[-T <: DFMember] extends DFTag

opaque type DFTags = Map[String, DFTag]
object DFTags:
  given CanEqual[DFTags, DFTags] = CanEqual.derived
  def empty: DFTags = Map()
  extension (tags: DFTags)
    def isEmpty: Boolean = tags.isEmpty
    def =~(that: DFTags): Boolean = tags == that
    private def tagName[CT <: DFTag: ClassTag]: String = classTag[CT].runtimeClass.getName()
    def tag[CT <: DFTag: ClassTag](customTag: CT): DFTags =
      tags + (tagName[CT] -> customTag)
    def removeTagOf[CT <: DFTag: ClassTag]: DFTags = tags - tagName[CT]
    def getTagOf[CT <: DFTag: ClassTag]: Option[CT] =
      tags.get(tagName[CT]).asInstanceOf[Option[CT]]
    def hasTagOf[CT <: DFTag: ClassTag]: Boolean =
      tags.contains(tagName[CT])
end DFTags

final case class NameTag(name: String) extends DFTag
case object DuplicateTag extends DFTagOf[DFDesignBlock]
type DuplicateTag = DuplicateTag.type
case object IteratorTag extends DFTagOf[DFVal.Dcl]
case object IdentTag extends DFTagOf[DFVal]
case object BindTag extends DFTagOf[DFVal]
case object CombinationalTag extends DFTagOf[DFBlock]
case class DefaultRTDomainCfgTag(cfg: RTDomainCfg.Explicit) extends DFTagOf[DFDesignBlock]
case object ExtendTag extends DFTagOf[DFVal]
type ExtendTag = ExtendTag.type
case object TruncateTag extends DFTagOf[DFVal]
type TruncateTag = TruncateTag.type
