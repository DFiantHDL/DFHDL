package DFiant.compiler.ir
import scala.reflect.{ClassTag, classTag}

sealed trait DFTag extends Product with Serializable
trait DFTagOf[-T <: DFMember] extends DFTag

opaque type DFTags = Map[ClassTag[_], DFTag]
object DFTags:
  def empty: DFTags = Map()
  extension (tags: DFTags)
    def tag[CT <: DFTag: ClassTag](customTag: CT): DFTags =
      tags + (classTag[CT] -> customTag)
    def removeTagOf[CT <: DFTag: ClassTag]: DFTags = tags - classTag[CT]
    def getTagOf[CT <: DFTag: ClassTag]: Option[CT] =
      tags.get(classTag[CT]).asInstanceOf[Option[CT]]
