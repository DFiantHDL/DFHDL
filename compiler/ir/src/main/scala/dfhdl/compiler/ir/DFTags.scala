package dfhdl.compiler.ir
import scala.reflect.{ClassTag, classTag}
import upickle.default.*

//TODO: check why enum is not working properly
sealed trait DFTag derives ReadWriter
case object IteratorTag extends DFTag
type IteratorTag = IteratorTag.type
case object IdentTag extends DFTag
type IdentTag = IdentTag.type
case object BindTag extends DFTag
type BindTag = BindTag.type
case object CombinationalTag extends DFTag
type CombinationalTag = CombinationalTag.type
case object FallThroughTag extends DFTag
type FallThroughTag = FallThroughTag.type
case class DefaultRTDomainCfgTag(
    clk: constraints.Timing.Clock,
    rst: constraints.Timing.Reset
) extends DFTag
case object ResizeTag extends DFTag
type ResizeTag = ResizeTag.type
// Width-adjustment PERMISSIONS carried by a value into a context that decides its width. Each
// permits one direction and contributes nothing in the other, where the context's ordinary
// width rules decide, error included: a tag is never a claim that an adjustment happens, only
// that one may. Which operand carries the tag is what an operation with no designated target
// (a comparison, unlike an assignment) has no other way of saying.
case object ExtendTag extends DFTag
type ExtendTag = ExtendTag.type
case object TruncateTag extends DFTag
type TruncateTag = TruncateTag.type
case object SyntheticDefaultTag extends DFTag
type SyntheticDefaultTag = SyntheticDefaultTag.type
case object ImplicitlyFromIntTag extends DFTag
type ImplicitlyFromIntTag = ImplicitlyFromIntTag.type
case class DFHDLVersionTag(version: String) extends DFTag
// Marks compiler-synthesized phantom members: ports/parameters (and their by-name
// selection and wiring artifacts) created to make methods self-contained when
// they use values from outside their own scope. The DFHDL printer hides
// phantom-tagged members in the method VIEW form only, so the printed def
// matches the user-written source (its body references the captured host values
// by name). Once a def is dropped to a regular design block, phantoms print like
// any other port/parameter, and the backend printers always keep them.
case object PhantomTag extends DFTag
type PhantomTag = PhantomTag.type

/** Marks a constant condition that an operation ASSUMED and elaboration could not prove, pending
  * materialization as a static assertion of the design's contract. A pure marker: the condition it
  * marks is the whole constraint, down to the text the assertion reports.
  *
  * Created and consumed inside one design's elaboration (the end of the design body materializes
  * every pending constraint and drops the tag), so no member of a finished design carries it. It is
  * therefore an elaboration-internal marker, not a stage marker: nothing downstream reads it, and
  * the materialized assertion is what carries the constraint from there on.
  */
case object AutoConstraint extends DFTag
type AutoConstraint = AutoConstraint.type

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
    def ++(that: DFTags): DFTags = tags ++ that
end DFTags
