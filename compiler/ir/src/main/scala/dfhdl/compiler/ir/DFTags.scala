package dfhdl.compiler.ir
import scala.reflect.{ClassTag, classTag}

sealed trait DFTag extends Product with Serializable
trait DFTagOf[-T <: DFMember] extends DFTag

opaque type DFTags = Map[ClassTag[?], DFTag]
object DFTags:
  given CanEqual[DFTags, DFTags] = CanEqual.derived
  def empty: DFTags = Map()
  extension (tags: DFTags)
    def isEmpty: Boolean = tags.isEmpty
    def =~(that: DFTags): Boolean = tags == that
    def tag[CT <: DFTag: ClassTag](customTag: CT): DFTags =
      tags + (classTag[CT] -> customTag)
    def removeTagOf[CT <: DFTag: ClassTag]: DFTags = tags - classTag[CT]
    def getTagOf[CT <: DFTag: ClassTag]: Option[CT] =
      tags.get(classTag[CT]).asInstanceOf[Option[CT]]
    def hasTagOf[CT <: DFTag: ClassTag]: Boolean =
      tags.contains(classTag[CT])

final case class NameTag(name: String) extends DFTag

case object DuplicateTag extends DFTagOf[DFDesignBlock]
type DuplicateTag = DuplicateTag.type

case object ToScalaValueAccess extends DFTagOf[DFVal]
type ToScalaValueAccess = ToScalaValueAccess.type
