package DFiant
import DFiant.DFAny.{Token, UninitializedDcl}
import DFiant.DFTuple.Token.ToFit
import internals._

import scala.reflect.macros.whitebox

object DFTuple {
  sealed abstract class Fields(implicit meta : Meta) extends DFStruct.Fields
  type Type[TplF <: Fields] = DFStruct.Type[TplF]
  type Token[TplF <: Fields] = DFStruct.TokenF[TplF]
  object Token {
    type ToFit[TplF <: Fields, V] = DFAny.Token.ToFit.Summon.SAM[Type[TplF], V, Token[TplF]]
  }

  sealed trait Frontend {
    protected implicit class __DFTupleInit[TplF <: Fields](
      val i : DFAny.UninitializedDcl[Type[TplF]]
    ) {
      def init[T <: Product](
        tokens : T
      )(implicit ctx : DFAny.Context) : DFAny.DclOf[Type[TplF]] = macro Frontend.initMacro[TplF]
      def __initTuple(tokens : DFAny.Token.ToFit.Conv[Type[TplF], i.dfType.TToken]*)(
        implicit ctx : DFAny.Context
      ) : DFAny.DclOf[Type[TplF]] = i.forcedInit(tokens.map(_.apply(i.dfType)))
    }
  }
  object Frontend {
    def initMacro[TplF <: Fields : c.WeakTypeTag](
      c : whitebox.Context
    )(tokens : c.Tree)(ctx : c.Tree) : c.Tree = {
      import c.universe._
      val tpe = weakTypeOf[TplF]
      val expectedTupleName = s"scala.Tuple${tpe.typeArgs.length}"
      val q"$ignore(..$tupleArgs)" = tokens
      val tokenArgs =
        if (tupleArgs.forall(t => t.tpe.typeSymbol.fullName == expectedTupleName)) tupleArgs
        else if (tokens.tpe.typeSymbol.fullName == expectedTupleName) List(tokens)
        else c.abort("Bad value")

      q"""${c.prefix.tree}.__initTuple(..$tokenArgs)"""
    }
    trait Inherited extends Frontend with DFTuple2.Frontend.Inherited with DFTuple3.Frontend.Inherited {
      final override protected implicit def __DFTupleInit[TplF <: Fields](i : UninitializedDcl[Type[TplF]]) : __DFTupleInit[TplF] = super.__DFTupleInit(i)
    }
    trait Imported extends Frontend with DFTuple2.Frontend.Imported with DFTuple3.Frontend.Imported {
      final override implicit def __DFTupleInit[TplF <: Fields](i : UninitializedDcl[Type[TplF]]) : __DFTupleInit[TplF] = super.__DFTupleInit(i)
    }
  }
}

object DFTuple2 {
  final class Fields[T1 <: DFAny.Type, T2 <: DFAny.Type](
    T1 : T1, T2 : T2
  ) extends DFTuple.Fields {
    final val _1 = T1 <> FIELD
    final val _2 = T2 <> FIELD
  }
  type Type[T1 <: DFAny.Type, T2 <: DFAny.Type] = DFStruct.Type[Fields[T1, T2]]

  trait Frontend {
    protected implicit def __DFTuple2Token[
      T1 <: DFAny.Type, T2 <: DFAny.Type,
      TT1, TT2,
      O1 <: DFAny.Token, O2 <: DFAny.Token
    ](
      implicit
      summonT1 : DFAny.Token.ToFit.Summon.SAM[T1, TT1, O1],
      summonT2 : DFAny.Token.ToFit.Summon.SAM[T2, TT2, O2],
    ) : DFTuple.Token.ToFit[Fields[T1, T2], Tuple2[TT1, TT2]] = (from, value) => {
      val token1 = summonT1(from.fields._1.dfType, value._1)
      val token2 = summonT2(from.fields._2.dfType, value._2)
      DFStruct.Token(from.fields, Map(
        from.fields._1 -> token1,
        from.fields._2 -> token2
      )).asInstanceOf[DFStruct.TokenF[Fields[T1, T2]]]
    }
  }
  object Frontend {
    trait Inherited extends Frontend {
      final override protected implicit def __DFTuple2Token[T1 <: DFAny.Type, T2 <: DFAny.Type, TT1, TT2, O1 <: Token, O2 <: Token](implicit summonT1 : Token.ToFit.Summon.SAM[T1, TT1, O1], summonT2 : Token.ToFit.Summon.SAM[T2, TT2, O2]) : ToFit[Fields[T1, T2], (TT1, TT2)] = super.__DFTuple2Token
    }
    trait Imported extends Frontend {
      final override implicit def __DFTuple2Token[T1 <: DFAny.Type, T2 <: DFAny.Type, TT1, TT2, O1 <: Token, O2 <: Token](implicit summonT1 : Token.ToFit.Summon.SAM[T1, TT1, O1], summonT2 : Token.ToFit.Summon.SAM[T2, TT2, O2]) : ToFit[Fields[T1, T2], (TT1, TT2)] = super.__DFTuple2Token
    }
  }
}

object DFTuple3 {
  final class Fields[T1 <: DFAny.Type, T2 <: DFAny.Type, T3 <: DFAny.Type](
    T1 : T1, T2 : T2, T3 : T3
  ) extends DFTuple.Fields {
    final val _1 = T1 <> FIELD
    final val _2 = T2 <> FIELD
    final val _3 = T3 <> FIELD
  }
  type Type[T1 <: DFAny.Type, T2 <: DFAny.Type, T3 <: DFAny.Type] = DFStruct.Type[Fields[T1, T2, T3]]

  trait Frontend {
    protected implicit def __DFTuple3Token[
      T1 <: DFAny.Type, T2 <: DFAny.Type, T3 <: DFAny.Type,
      TT1, TT2, TT3,
      O1 <: DFAny.Token, O2 <: DFAny.Token, O3 <: DFAny.Token
    ](
      implicit
      summonT1 : DFAny.Token.ToFit.Summon.SAM[T1, TT1, O1],
      summonT2 : DFAny.Token.ToFit.Summon.SAM[T2, TT2, O2],
      summonT3 : DFAny.Token.ToFit.Summon.SAM[T3, TT3, O3],
    ) : DFTuple.Token.ToFit[Fields[T1, T2, T3], Tuple3[TT1, TT2, TT3]] = (from, value) => {
      val token1 = summonT1(from.fields._1.dfType, value._1)
      val token2 = summonT2(from.fields._2.dfType, value._2)
      val token3 = summonT3(from.fields._3.dfType, value._3)
      DFStruct.Token(from.fields, Map(
        from.fields._1 -> token1,
        from.fields._2 -> token2,
        from.fields._3 -> token3,
      )).asInstanceOf[DFStruct.TokenF[Fields[T1, T2, T3]]]
    }
  }
  object Frontend {
    trait Inherited extends Frontend {
      final override protected implicit def __DFTuple3Token[T1 <: DFAny.Type, T2 <: DFAny.Type, T3 <: DFAny.Type, TT1, TT2, TT3, O1 <: Token, O2 <: Token, O3 <: Token](implicit summonT1 : Token.ToFit.Summon.SAM[T1, TT1, O1], summonT2 : Token.ToFit.Summon.SAM[T2, TT2, O2], summonT3 : Token.ToFit.Summon.SAM[T3, TT3, O3]) : ToFit[Fields[T1, T2, T3], (TT1, TT2, TT3)] = super.__DFTuple3Token
    }
    trait Imported extends Frontend {
      final override implicit def __DFTuple3Token[T1 <: DFAny.Type, T2 <: DFAny.Type, T3 <: DFAny.Type, TT1, TT2, TT3, O1 <: Token, O2 <: Token, O3 <: Token](implicit summonT1 : Token.ToFit.Summon.SAM[T1, TT1, O1], summonT2 : Token.ToFit.Summon.SAM[T2, TT2, O2], summonT3 : Token.ToFit.Summon.SAM[T3, TT3, O3]) : ToFit[Fields[T1, T2, T3], (TT1, TT2, TT3)] = super.__DFTuple3Token
    }
  }
}
