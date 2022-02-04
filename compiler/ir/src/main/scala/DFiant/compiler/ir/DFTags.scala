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
    def contains[CT <: DFTag: ClassTag]: Boolean =
      tags.contains(classTag[CT])

final case class ExternalInit(tokenSeq: List[DFTokenAny]) extends DFTagOf[DFVal]
final case class NameTag(name: String) extends DFTag
sealed trait Domain extends DFTagOf[DFOwner]
object Domain:
  sealed trait DF extends Domain
  case object DF extends DF
  sealed trait RT extends Domain
  case object RT extends RT
