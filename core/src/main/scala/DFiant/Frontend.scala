package DFiant

export core.DFType.Ops.*
export core.DFTokenOps.*
//export core.DFBoolOrBit.Token.Ops.*
//export core.DFBoolOrBit.Ops.*
export core.DFBits.Token.Ops.*
export core.DFBits.Token.StrInterp.*
export core.DFBits.Val.Ops.*
export core.DFDecimal.Token.StrInterp.*
export core.DFUInt.Token.Ops.*
export core.DFUInt.Ops.*
export core.DFSInt.Ops.*
export core.DFUnion.Ops.*
export core.DFOpaque.Ops.*
export core.DFVector.Ops.*
export core.DFVal.Ops.*
export core.DFVarOps.*
export internals.CommonOps.*

type DFC = core.DFC
export core.dfc

type DFType = core.DFType
val DFBool = core.DFBool
type DFBool = core.DFBool
val DFBit = core.DFBit
type DFBit = core.DFBit
type DFBits[W <: Int] = core.DFBits[W]
type DFUInt[W <: Int] = core.DFUInt[W]
val DFUInt = core.DFUInt
type DFSInt[W <: Int] = core.DFSInt[W]
val DFSInt = core.DFSInt
val DFBits = core.DFBits
type DFFields = core.DFFields
type DFEncoding = core.DFEncoding
val DFEncoding = core.DFEncoding
type DFDesign = core.DFDesign

val IN = core.IN
val OUT = core.OUT
val INOUT = core.INOUT
val VAR = core.VAR
type VAL = core.VAL
type VAR = core.VAR
type IN = core.IN
type OUT = core.OUT
type TOKEN = core.TOKEN
type <>[T <: DFType, M] = core.<>[T, M]

val ? = core.?
export core.SameBitsVector.*

trait OpaqueTest[T]
object OpaqueTest:
  import scala.quoted.*
  transparent inline given from[T]: OpaqueTest[T] = ${ testMacro[T] }
  def testMacro[T](using Quotes, Type[T]): Expr[OpaqueTest[T]] =
    import quotes.reflect.*
    val t = TypeRepr.of[T]
    val tupleTpe = TypeRepr.of[Tuple]
    def checkOpaque(t: TypeRepr): Unit =
      t.dealias match
        case t: AppliedType if t <:< tupleTpe =>
          t.args.foreach(checkOpaque)
        case t: TypeRef if t.isOpaqueAlias => //OK
          println(t)
        case t =>
          report.error(s"Found non-opaque type: ${t}")

    checkOpaque(t)
    '{ new OpaqueTest[T] {} }
  end testMacro
end OpaqueTest
