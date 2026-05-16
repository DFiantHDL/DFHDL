package dfhdl.lib.arith.constdiv
import dfhdl.*
// scalafmt: { align.tokens = [{code = "<>"}, {code = "="}, {code = "=>"}, {code = ":="}]}

given options.CompilerOptions.LogLevel = _.TRACE
class PrimeDiv(
    val primeDivisor: Int <> CONST
) extends RTDesign:
  assert(primeDivisor > 1, "Prime divisor must be greater than 1")
  val DIVIDEND_WIDTH = clog2(primeDivisor + 1) + 1
  val dividend       = UInt(DIVIDEND_WIDTH)  <> IN
  val quotient       = Bit                   <> OUT.REG
  val remainder      = UInt.to(primeDivisor) <> OUT.REG

  val diff      = dividend - primeDivisor
  val underflow = diff(DIVIDEND_WIDTH - 1)
  quotient.din  := underflow
  remainder.din := underflow.sel(primeDivisor, diff.resize(clog2(primeDivisor)))
end PrimeDiv

@top class PrimeDiv5 extends RTDesign:
  val dividend  = UInt(4) <> VAR.REG init 0
  val primeDiv5 = PrimeDiv(5)
  primeDiv5.dividend <> dividend
  dividend.din       := dividend + 1
  println(
    s"dividend: ${dividend.reg(1, init = ?)}, quotient: ${primeDiv5.quotient}, remainder: ${primeDiv5.remainder}"
  )
