package dfhdl.sim
import dfhdl.toScalaBoolean
import dfhdl.core.{DFConstOf, DFTypeAny, DFVal, DFCG, CONST, dfType, DFValAny}
import dfhdl.internals.NoTopAnnotIsRequired
import dfhdl.compiler.ir
import ir.DFVal.Func.Op as FuncOp
import munit.*
import munit.diff.{DiffOptions, Printer}

/** munit base for DFacsimile simulation specs: `assertEquals` just works on DFHDL constants — any
  * two constants (including test-side constant arithmetic like `(iv.uint + x.peek.uint)`) compare
  * bit-accurately, so tests never touch packed integers. Designs are instantiated directly for
  * simulation, so no `@top` annotation is required.
  */
abstract class SimSpec extends munit.FunSuite, NoTopAnnotIsRequired:
  val dfc: DFCG = DFCG()
  given DFCG = dfc
  override val printer = new Printer:
    def print(value: Any, out: StringBuilder, indent: Int): Boolean =
      value match
        case dfVal: DFValAny =>
          import dfc.getSet
          dfVal.asIR match
            case constIR: ir.DFVal if constIR.isConst =>
              import dfhdl.compiler.printing.{DefaultPrinter, Printer}
              val data = constIR.getConstDataOrDefault[Any]
              given printer: Printer = DefaultPrinter
              out.append(printer.csConstData(constIR.dfType, data))
              true
            case _ => false
        case _ => false

  given [T1 <: DFTypeAny, V](using
      tc: DFVal.Compare[T1, V, FuncOp.===.type, false] { type OutP = CONST },
      dfc: DFCG
  ): munit.Compare[DFConstOf[T1], V] with
    def isEqual(obtained: DFConstOf[T1], expected: V): Boolean =
      tc(obtained, expected).toScalaBoolean
  end given

  protected def bothTiers(name: String)(body: SimTier => Unit): Unit =
    for tier <- List(SimTier.Interpreter, SimTier.Codegen) do
      test(s"$name [$tier]")(body(tier))
end SimSpec
