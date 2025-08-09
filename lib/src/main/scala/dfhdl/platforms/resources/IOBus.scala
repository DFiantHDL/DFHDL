package dfhdl.platforms.resources
import dfhdl.internals.*
import dfhdl.compiler.ir.constraints
import scala.quoted.*
import dfhdl.core.*
import dfhdl.bits
import Resource.CanConnect

class IOBus[T <: IO, L <: Int] private (val ios: List[T]) extends ResourceDeps:
  def apply(i: Int): T = ios(i)
  lazy val upstreamDeps: List[Resource] = ios
  @dfhdl.internals.metaContextIgnore
  override protected[dfhdl] def connect(that: DFValAny)(using dfc: DFC): Unit =
    import dfc.getSet
    assert(ios.length == that.asIR.width, "Width mismatch")
    for (i <- 0 until ios.length)
      ios(i).connect(that.bits(i))
  end connect
object IOBus:
  def fill[L <: Int & Singleton](length: L)(f: DFC ?=> IO)(using dfc: DFC): IOBus[IO, L] =
    new IOBus[IO, L](List.tabulate(length)(i => f(using dfc.setName(s"P$i"))))
  private def forced[T <: IO, L <: Int](ios: List[T])(using DFC): IOBus[T, L] =
    new IOBus[T, L](ios.toList)
  transparent inline def apply[T <: IO](inline ios: T*): IOBus[T, ?] = ${ applyMacro[T]('ios) }
  def applyMacro[T <: IO](ios: Expr[Seq[T]])(using Quotes, Type[T]): Expr[IOBus[T, ?]] =
    import quotes.reflect.*
    val Varargs(args) = ios: @unchecked
    val ctLength = args.length
    val ctLengthType = ConstantType(IntConstant(ctLength)).asTypeOf[Int]
    '{
      forced[T, ctLengthType.Underlying]($ios.toList)(using compiletime.summonInline[DFC])
    }
  protected object `RW == TW`
      extends Check2[
        Int,
        Int,
        [RW <: Int, TW <: Int] =>> RW == TW,
        [RW <: Int, TW <: Int] =>> "The resource width (" + RW +
          ") is different than the DFHDL value width (" + TW + ")."
      ]
  given [RT <: IO, RL <: Int, R <: IOBus[RT, RL], T <: DFTypeAny, V <: DFValOf[T]](using
      dfc: DFC,
      wT: Width[T]
  )(using
      check: AssertGiven[RL =:= wT.OutI, "Width mismatch"]
  ): CanConnect[R, V] = (resource: R, dfVal: V) => resource.connect(dfVal)
end IOBus
