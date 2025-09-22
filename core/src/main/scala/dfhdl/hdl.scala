package dfhdl
protected object hdl:
  class dsn extends scala.annotation.StaticAnnotation
  import core.IntP
  export core.DFType.Ops.*
  export core.DFBoolOrBit.Val.Ops.*
  export core.DFBits.StrInterpOps.{h, b}
  export core.DFBits.Val.Ops.*
  export core.DFBits.Val.TupleOps.*
  export core.DFDecimal.StrInterpOps.{d, sd}
  export core.DFDecimal.Val.Ops.*
  export core.DFEnum.Val.Ops.*
  export core.DFVector.Val.Ops.*
  export core.DFOpaque.Val.Ops.*
  export core.DFTuple.Val.Ops.*
  export core.DFVector.Ops.*
  export core.TDFDouble.Val.Ops.*
  export core.TDFString.Val.Ops.*
  export core.DFVal.Ops.*
  export core.DFVarOps.*
  export core.DFPortOps.*
  export core.Conditional.Ops.*
  export core.TextOut.Ops.*
  export compiler.ir.TextOut.Severity
  export internals.CommonOps.*
  export core.{dfType}
  export core.DFPhysical.Val.Ops.*
  export platforms.resources.Resource.Ops.*
  export core.COMB_LOOP
  type Time = core.DFTime
  val Time = core.DFTime
  type Freq = core.DFFreq
  val Freq = core.DFFreq
  export core.DFRange.Ops.*
  export core.Step.Ops.*
  export core.Wait.Ops.*
  export core.RTDomainContainer.Ops.*
  type Step = core.Step
  type DFC = core.DFC
  val DFC = core.DFC
  export core.dfc
  export internals.Inlined
  type DFType = core.DFTypeAny
  lazy val Bit = core.DFBit
  type Bit = core.DFBit
  type Bits[W <: IntP] = core.DFBits[W]
  val Bits = core.DFBits
  type UInt[W <: IntP] = core.DFUInt[W]
  val UInt = core.DFUInt
  type SInt[W <: IntP] = core.DFSInt[W]
  val SInt = core.DFSInt
  type Encoded = core.DFEncoding.Default
  val Encoded = core.DFEncoding
  export core.DFStruct.Fields as Struct
  export core.DFOpaque.Frontend as Opaque
  export core.DFOpaque.{Magnet, Clk, Rst}
  type DFDesign = core.DFDesign
  type RTDesign = core.RTDesign
  type EDDesign = core.EDDesign
  type EDBlackBox = core.EDBlackBox
  val EDBlackBox = core.EDBlackBox
  type DFDomain = core.DFDomain
  type RTDomain = core.RTDomain
  type EDDomain = core.EDDomain
  export compiler.ir.InitFileFormat
  export compiler.ir.InitFileUndefinedValue

  val IN = core.Modifier.IN
  val OUT = core.Modifier.OUT
  val INOUT = core.Modifier.INOUT
  val VAR = core.Modifier.VAR
  type VAL = core.VAL
  type CONST = core.CONST
  type DFRET = core.DFRET
  type RTRET = core.RTRET
  type EDRET = core.EDRET
  val OPEN = core.DFVal.OPEN
  val NOTHING = core.DFVal.NOTHING
  export core.DFVal.CLK_FREQ
  export core.<>
  export core.X
  export core.Process.Ops.*

  type ClkCfg = core.ClkCfg
  val ClkCfg = core.ClkCfg
  type RstCfg = core.RstCfg
  val RstCfg = core.RstCfg
  type RTDomainCfg = core.RTDomainCfg
  val RTDomainCfg = core.RTDomainCfg

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
