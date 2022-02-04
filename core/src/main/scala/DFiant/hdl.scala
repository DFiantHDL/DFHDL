package DFiant

object hdl:
  export core.DFType.Ops.*
  export core.DFToken.Ops.*
  export core.Bit
  export core.DFBoolOrBit.Token.Ops.*
  export core.DFBoolOrBit.Val.Ops.*
  export core.DFBits.Token.Ops.*
  export core.DFBits.Token.StrInterp.{h, b}
  export core.DFBits.Val.Ops.*
  export core.DFDecimal.Token.StrInterp.{d, sd}
  export core.DFDecimal.Token.Ops.*
  export core.DFDecimal.Val.Ops.*
  export core.DFVector.Token.Ops.*
  export core.DFVector.Val.Ops.*
  export core.DFOpaque.Frontend as DFOpaque
  export core.DFOpaque.Token.Ops.*
  export core.DFOpaque.Val.Ops.*
  export core.DFTuple.Token.Ops.*
  export core.DFTuple.Val.Ops.*
  export core.DFVector.Ops.*
  export core.DFVal.Ops.*
  export core.DFVarOps.*
  export core.DFPortOps.*
  export internals.CommonOps.*
  export core.{width, dfType}

  type DFC = core.DFC
  export core.dfc

  type DFType = core.DFTypeAny
  lazy val DFBool = core.DFBool
  type DFBool = core.DFBool
  lazy val DFBit = core.DFBit
  type DFBit = core.DFBit
  type DFBits[W <: Int] = core.DFBits[W]
  type DFUInt[W <: Int] = core.DFUInt[W]
  val DFUInt = core.DFUInt
  type DFSInt[W <: Int] = core.DFSInt[W]
  val DFSInt = core.DFSInt
  val DFBits = core.DFBits
  type DFEnum = core.DFEncoding.Default
  val DFEnum = core.DFEncoding
  export core.DFStruct.Fields as DFStruct
  type DFDesign = core.DFDesign
  type RTDesign = core.RTDesign

  val IN = core.IN
  val OUT = core.OUT
  val VAR = core.VAR
  type VAL = core.VAL
  type TOKEN = core.TOKEN
  export core.<>

  // shorthand for annotating a DFBits value (useful for string interpolation)
  type B[W <: Int] = core.DFValOf[DFBits[W]]
  val ? = core.?
  export core.SameElementsVector as all
  extension [Entry <: core.DFEncoding](e: Entry)
    def unapply[E >: Entry <: core.DFEncoding](arg: core.DFValOf[core.DFEnum[E]])(using
        DFC
    ): Boolean =
      ???

  implicit def __refined_token[T <: core.FieldsOrTuple](
      token: core.DFToken[core.DFStruct[T]]
  )(using
      r: core.DFToken.Refiner[T]
  ): r.Out = token.asInstanceOf[r.Out]
end hdl

export hdl.*
