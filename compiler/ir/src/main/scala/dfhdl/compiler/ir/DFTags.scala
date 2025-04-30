package dfhdl.compiler.ir
import scala.reflect.{ClassTag, classTag}

sealed trait DFTag extends Product with Serializable
trait DFTagOf[-T <: DFMember] extends DFTag

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

final case class NameTag(name: String) extends DFTag

case object DuplicateTag extends DFTagOf[DFDesignBlock]
type DuplicateTag = DuplicateTag.type

case object ToScalaValueAccess extends DFTagOf[DFVal]
type ToScalaValueAccess = ToScalaValueAccess.type
