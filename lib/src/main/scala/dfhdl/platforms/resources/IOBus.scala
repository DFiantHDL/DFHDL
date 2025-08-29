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
  def reverse(using DFC): IOBus[T, L] =
    val ret = new IOBus[T, L](ios.reverse)
    ret.injectID(id + ".reverse")
    for (c <- getResourceConstraints)
      ret.injectConstraint(c)
    ret
object IOBus:
  def fill[T <: IO, L <: Int & Singleton](length: L)(f: => T)(using dfc: DFC): IOBus[T, L] =
    forced[T, L](List.tabulate(length)(i => f.injectID(s"${dfc.getMeta.name}($i)")))
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
      check: AssertGiven[RL =:= wT.OutI, "Width mismatch"],
      cc: CanConnect[RT, DFValOf[DFBit]]
  ): CanConnect[R, V] = (resource: R, dfVal: V) =>
    for (i <- 0 until resource.ios.length)
      cc.connect(resource.ios(i), dfVal.bits(i))
end IOBus
