package dfhdl.compiler.ir
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
    def hasTagOf[CT <: DFTag: ClassTag]: Boolean =
      tags.contains(classTag[CT])

final case class ExternalInit(tokenSeq: List[DFTokenAny]) extends DFTagOf[DFVal]
final case class NameTag(name: String) extends DFTag
final case class Unused(action: Unused.Action) extends DFTagOf[DFVal]
object Unused:
  enum Action derives CanEqual:
    /** Quiet just suppresses the unused warning for the tagged value.
      */
    case Quiet

    /** Keep suppresses the unused warning, and also attempts to keep the tagged value.
      */
    case Keep

    /** Purge removes all the redundant paths until and including the tagged value.
      */
    case Purge
