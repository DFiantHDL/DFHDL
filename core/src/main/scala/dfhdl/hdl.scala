package dfhdl
protected object hdl:
  class dsn extends scala.annotation.StaticAnnotation
  export core.DFType.Ops.*
  export core.DFToken.Ops.*
  export core.DFBoolOrBit.Token.Ops.*
  export core.DFBoolOrBit.Val.Ops.*
  export core.DFBits.Token.Ops.*
  export core.DFBits.Token.StrInterp.{h, b}
  export core.DFBits.Val.Ops.*
  export core.DFBits.Val.TupleOps.*
  export core.DFDecimal.Token.StrInterp.{d, sd}
  export core.DFDecimal.Token.Ops.*
  export core.DFDecimal.Val.Ops.*
  export core.DFVector.Token.Ops.*
  export core.DFVector.Val.Ops.*
  export core.DFOpaque.Token.Ops.*
  export core.DFOpaque.Val.Ops.*
  export core.DFTuple.Token.Ops.*
  export core.DFTuple.Val.Ops.*
  export core.DFVector.Ops.*
  export core.DFVal.Ops.*
  export core.DFVarOps.*
  export core.DFPortOps.*
  export core.Conditional.Ops.*
  export internals.CommonOps.*
  export core.{width, dfType}
  export core.Timer.Ops.*
  export core.Wait.Ops.*
  type Timer = core.Timer
  val Timer = core.Timer
  type DFC = core.DFC
  val DFC = core.DFC
  export core.dfc
  export internals.Inlined
  export core.hw
  type DFType = core.DFTypeAny
  lazy val Bit = core.DFBit
  type Bit = core.DFBit
  type Bits[W <: Int] = core.DFBits[W]
  val Bits = core.DFBits
  type UInt[W <: Int] = core.DFUInt[W]
  val UInt = core.DFUInt
  type SInt[W <: Int] = core.DFSInt[W]
  val SInt = core.DFSInt
  type Encode = core.DFEncoding.Default
  val Encode = core.DFEncoding
  export core.DFStruct.Fields as Struct
  export core.DFOpaque.Frontend as Opaque
  type DFDesign = core.DFDesign
  type RTDesign = core.RTDesign
  type EDDesign = core.EDDesign
  type EDBlackBox = core.EDBlackBox
  val EDBlackBox = core.EDBlackBox
  type DFDomain = core.DFDomain
  type RTDomain = core.RTDomain
  type EDDomain = core.EDDomain

  val IN = core.Modifier.IN
  val OUT = core.Modifier.OUT
  val INOUT = core.Modifier.INOUT
  val VAR = core.Modifier.VAR
  type VAL = core.VAL
  type TOKEN = core.TOKEN
  type DFRET = core.DFRET
  export core.<>
  export core.X
  export core.OPEN
  export core.Process.Ops.*

  val DerivedCfg = core.DerivedCfg
  type ClkCfg = core.ClkCfg
  val ClkCfg = core.ClkCfg
  type RstCfg = core.RstCfg
  val RstCfg = core.RstCfg
  type RTDomainCfg = core.RTDomainCfg
  val RTDomainCfg = core.RTDomainCfg
  val NoClockCfg = RTDomainCfg(None, None)

  // shorthand for annotating a DFBits value (useful for string interpolation)
  type B[W <: Int] = core.DFValOf[Bits[W]]
  val ? = core.?
  export core.SameElementsVector as all
  extension [Entry <: core.DFEncoding](e: Entry)
    def unapply[E >: Entry <: core.DFEncoding](arg: core.DFValOf[core.DFEnum[E]])(using
        DFC
    ): Boolean =
      ???
  extension (dfVal: core.DFValAny)
    inline def asValOf[T <: core.DFType.Supported]: T <> VAL =
      dfVal.asInstanceOf[T <> VAL]
  extension (dfVal: compiler.ir.DFVal)
    inline def asValOf[T <: core.DFType.Supported]: T <> VAL =
      core.DFVal(dfVal).asInstanceOf[T <> VAL]

  import java.util.Properties

  val dfhdlVersion: String =
    val props = new Properties()
    val inputStream = getClass.getClassLoader.getResourceAsStream("version.properties")
    props.load(inputStream)
    props.getProperty("version")
end hdl

export hdl.*
