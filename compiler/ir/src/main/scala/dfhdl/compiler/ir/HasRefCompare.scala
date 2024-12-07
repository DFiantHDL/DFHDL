package dfhdl.compiler.ir

trait HasRefCompare[T <: HasRefCompare[T]]:
  private var cachedCompare: Option[(T, Boolean)] = None
  final def =~(that: T)(using MemberGetSet): Boolean =
    cachedCompare match
      case Some(prevCompare, result) if prevCompare eq that => result
      case _ =>
        val res = this `prot_=~` that
        cachedCompare = Some(that, res)
        res
  protected def `prot_=~`(that: T)(using MemberGetSet): Boolean
  lazy val getRefs: List[DFRef.TwoWayAny]
  def copyWithNewRefs: this.type
