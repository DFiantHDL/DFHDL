package DFiant
import DFiant.DFAny.UninitializedDcl
import DFiant.compiler.csprinter.CSPrinter
import internals._
import singleton.ops.RequireMsg

object DFOpaque {
  trait Fields extends DFStruct.Fields {
    type TType = DFOpaque.Type[this.type]
    type TToken = DFOpaque.TokenF[this.type]
    type TVal = DFAny.Of[TType]
    type AsIs[Mod <: DFAny.Modifier] = DFAny.Value[TType, Mod]
    type ActualType <: DFAny.Type
    val actualType : ActualType
    type Width = actualType.Width
    final lazy val width : TwoFace.Int[Width] = actualType.width
    final val actual = actualType <> FIELD
    def codeString(implicit printer: CSPrinter) : String = name
  }
  abstract class Of[T <: DFAny.Type, TT](template : TT)(
    implicit meta : Meta, tc : TT => T, r : RequireMsg[false, "Missing @df annotation"]
  ) extends Fields {
    type ActualType = T
    final lazy val actualType = tc(template)
  }
  type Type[F <: Fields] = DFStruct.Type[F]
  object Type {
    def unapply(dfType : DFStruct.Type[_]) : Option[DFAny.Type] = dfType.fields match {
      case opaqueFields : Fields => Some(opaqueFields.actualType)
      case _ => None
    }
  }
  type TokenF[F <: Fields] = DFStruct.TokenF[F]
  final implicit class OpaqueOps[F <: Fields](fields : F) {
    def apply[T](t : DFAny.Token.ToFit.Conv[fields.ActualType, fields.ActualType#TToken])(
      implicit w : DFStruct.Fields.WidthOf[F]
    ) : TokenF[F] = {
      DFStruct.Token(fields, Map(fields.actual -> t(fields.actualType))).asInstanceOf[TokenF[F]]
    }
  }
  protected[DFiant] trait Frontend {
    protected implicit class __DFOpaqueInit[F <: Fields](
      val left : DFAny.UninitializedDcl[Type[F]]
    ) {
      def init(tokenSeq : TokenF[F]*)(
        implicit ctx : DFAny.Context, di : DummyImplicit
      ) : DFAny.DclOf[Type[F]] = {
        left.forcedInit(tokenSeq)
      }
    }
  }
  object Frontend {
    trait Inherited extends Frontend {
      final override protected implicit def __DFOpaqueInit[F <: Fields](left : UninitializedDcl[Type[F]]) : __DFOpaqueInit[F] = super.__DFOpaqueInit(left)
    }
    trait Imported extends Frontend {
      final override implicit def __DFOpaqueInit[F <: Fields](left : UninitializedDcl[Type[F]]) : __DFOpaqueInit[F] = super.__DFOpaqueInit(left)
    }
  }
}

