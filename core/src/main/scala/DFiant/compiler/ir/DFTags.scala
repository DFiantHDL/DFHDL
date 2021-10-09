package DFiant.compiler.ir
import scala.reflect.{ClassTag, classTag}

sealed trait DFTag extends Product with Serializable
trait DFTagOf[-T <: DFMember] extends DFTag

opaque type DFTags = Map[ClassTag[_], DFTag]
object DFTags:
  given CanEqual[DFTags, DFTags] = CanEqual.derived
  def empty: DFTags = Map()
  extension (tags: DFTags)
    def =~(that: DFTags): Boolean = tags == that
    def tag[CT <: DFTag: ClassTag](customTag: CT): DFTags =
      tags + (classTag[CT] -> customTag)
    def removeTagOf[CT <: DFTag: ClassTag]: DFTags = tags - classTag[CT]
    def getTagOf[CT <: DFTag: ClassTag]: Option[CT] =
      tags.get(classTag[CT]).asInstanceOf[Option[CT]]

final case class ExternalInit(tokenSeq: Seq[DFType.Token])
    extends DFTagOf[DFVal]
