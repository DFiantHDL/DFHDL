package DFiant.core

trait DFEnum[E <: Enumeration#Value] extends DFAny.ValW[WUnsafe, DFEnum[E],DFEnum.Var[E]] {
  protected val enum : Enumeration
  def == (that : E) : DFBool = ???
  val width = wOF(enum.maxId)
  def dfTypeName : String = "DFEnum"
}

object DFEnum {
  case class Var[E <: Enumeration#Value](protected val enum : Enumeration) extends DFAny.VarW[WUnsafe, DFEnum[E],DFEnum.Var[E]] with DFEnum[E] {
    def := (that : E) : Unit = {}
    def newEmptyDFVar = copy()
  }

  def apply(enum : Enumeration) = Var[enum.Value](enum)
}

