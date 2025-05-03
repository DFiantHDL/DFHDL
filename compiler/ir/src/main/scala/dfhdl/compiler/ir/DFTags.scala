package dfhdl.compiler.ir
import scala.reflect.{ClassTag, classTag}
import upickle.default.*

//TODO: check why enum is not working properly
sealed trait DFTag derives ReadWriter
case class NameTag(name: String) extends DFTag
case object DuplicateTag extends DFTag
type DuplicateTag = DuplicateTag.type
case object IteratorTag extends DFTag
type IteratorTag = IteratorTag.type
case object IdentTag extends DFTag
type IdentTag = IdentTag.type
case object BindTag extends DFTag
type BindTag = BindTag.type
case object CombinationalTag extends DFTag
type CombinationalTag = CombinationalTag.type
case class DefaultRTDomainCfgTag(cfg: RTDomainCfg.Explicit) extends DFTag
case object ExtendTag extends DFTag
type ExtendTag = ExtendTag.type
case object TruncateTag extends DFTag
type TruncateTag = TruncateTag.type

opaque type DFTags = Map[String, DFTag]
object DFTags:
  given ReadWriter[DFTags] = summon[ReadWriter[Map[String, DFTag]]]
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
