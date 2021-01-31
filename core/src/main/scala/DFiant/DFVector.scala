package DFiant

import singleton.ops._
import singleton.twoface._
import DFiant.internals._
import DFiant.DFAny.{Func2, Modifier, TokenT, `Op==,!=`}
import DFiant.DFVector.`LN == RN`
import compiler.csprinter._

object DFVector extends DFAny.Companion {
  final case class Type[T <: DFAny.Type, N](cellType : T, cellNum : TwoFace.Int[N]) extends DFAny.Type {
    type CellNum = N
    type Width = cellType.Width * N
    type TToken = Token
    type TPattern = Nothing
    type TPatternAble[+R] = Nothing
    type TPatternBuilder[LType <: DFAny.Type] = Nothing
    val width : TwoFace.Int[Width] = TwoFace.Int.create[Width](cellType.width * cellNum)
    def getBubbleToken: TToken = Token.bubble(cellType, cellNum)
    def getTokenFromBits(fromToken : DFBits.Token) : DFAny.Token = {
      assert(fromToken.width == width.getValue)
      val cellNum = fromToken.width / cellType.width
      val cells = for (i <- 0 until cellNum) yield fromToken.bitsWL(cellType.width, i * cellType.width)
      Token(cellType, cells.toVector)
    }
    def assignCheck(from : DFAny.Member)(implicit ctx : DFAny.Context) : Unit = trydf {
      from match {
        case r @ DFVector(cellType, cellNum) if cellType == this.cellType && cellNum == this.cellNum.getValue =>
      }
    }
    def codeString(implicit printer: CSPrinter) : String = {
      import printer.config._
      s"${cellType.codeString}.$DF X($LIT$cellNum)"
    }
    override def equals(obj: Any): Boolean = obj match {
      case Type(cellType, cellNum) => this.cellType == cellType && this.cellNum.getValue == cellNum.getValue
      case _ => false
    }
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Public Constructors
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  def unapply(arg: DFAny.Member): Option[(DFAny.Type, Int)] = arg.dfType match {
    case Type(cellType, cellNum) => Some(cellType, cellNum)
    case _ => None
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Implicits
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  object Frontend {
    trait Inherited extends Op.Frontend.Inherited with `Op:=,<>`.Frontend.Inherited with Token.Frontend.Inherited
    trait Imported extends Op.Frontend.Imported with `Op:=,<>`.Frontend.Imported with Token.Frontend.Imported
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Token
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  type TokenN[T <: DFAny.Type, N] = DFAny.TokenT[Token, Type[T, N]]
  final case class Token(cellType : DFAny.Type, value : Vector[DFAny.Token]) extends DFAny.Token { left =>
    val dfType : DFAny.Type = Type(cellType, value.length)
    val width : Int = value.length * cellType.width
    lazy val valueBits : BitVector = value.view.map(_.valueBits).reduce(_ ++ _)
    lazy val bubbleMask: BitVector = value.view.map(_.bubbleMask).reduce(_ ++ _)
    def ++ (that : Token) : Token = {
      assert(cellType == that.cellType)
      Token(cellType, this.value ++ that.value)
    }
    def sel(idx : DFUInt.Token) : DFAny.Token = idx.value match {
      case Some(i) => value(i.toInt)
      case None => cellType.getBubbleToken
    }
    def == (that : DFAny.Token)(implicit bb : Bubble.Behaviour) : DFBool.Token = that match {
      case right : Token =>
        DFBool.Token(logical = true, this.value equals right.value)
      case _ => ???
    }

    def codeString(implicit printer: CSPrinter) : String = {
      import printer.config._
      s"""Vector(${value.map(_.codeString).mkString(", ")})"""
    }
  }

  object Token {
    def bubble(cellType : DFAny.Type, cellNum : Int) : Token =
      Token(cellType, Vector.fill(cellNum)(cellType.getBubbleToken))

    type ToFit[LT <: DFAny.Type, LN, V] = DFAny.Token.ToFit.Summon.SAM[Type[LT, LN], Vector[V], TokenN[LT, LN]]
    type AsIs[LT <: DFAny.Type, LN, V] = DFAny.Token.AsIs.Summon.SAM[Type[LT, LN], Vector[V], TokenN[LT, LN]]
    sealed trait Frontend {
      protected implicit def __DFVectorTokenVectorToFit[LT <: DFAny.Type, LN, V](
        implicit
        tokenOf : DFAny.Token.ToFit.Summon[LT, V, DFAny.TokenT[LT#TToken, LT]],
        sameLength : `LN == RN`.CheckedShell[LN, Int]
      ) : ToFit[LT, LN, V] = (from, value) => {
        sameLength.unsafeCheck(from.cellNum, value.length)
        val tokenVec : Vector[DFAny.Token] = value.map(tokenOf(from.cellType, _))
        Token(from.cellType, tokenVec).typeTag[Type[LT, LN]]
      }
      protected implicit def __DFVectorTokenVectorAsIs[LT <: DFAny.Type, LN, V](
        implicit
        tokenOf : DFAny.Token.AsIs.Summon[LT, V, DFAny.TokenT[LT#TToken, LT]],
        sameLength : `LN == RN`.CheckedShell[LN, Int]
      ) : AsIs[LT, LN, V] = (from, value) => {
        sameLength.unsafeCheck(from.cellNum, value.length)
        val tokenVec : Vector[DFAny.Token] = value.map(tokenOf(from.cellType, _))
        Token(from.cellType, tokenVec).typeTag[Type[LT, LN]]
      }
    }
    object Frontend {
      trait Inherited extends Frontend {
        final override protected implicit def __DFVectorTokenVectorToFit[LT <: DFAny.Type, LN, V](implicit tokenOf : DFAny.Token.ToFit.Summon[LT, V, TokenT[LT#TToken, LT]], sameLength : `LN == RN`.CheckedShell[LN, Int]) : ToFit[LT, LN, V] = super.__DFVectorTokenVectorToFit
        final override protected implicit def __DFVectorTokenVectorAsIs[LT <: DFAny.Type, LN, V](implicit tokenOf : DFAny.Token.AsIs.Summon[LT, V, TokenT[LT#TToken, LT]], sameLength : `LN == RN`.CheckedShell[LN, Int]) : AsIs[LT, LN, V] = super.__DFVectorTokenVectorAsIs
      }
      trait Imported extends Frontend {
        final override implicit def __DFVectorTokenVectorToFit[LT <: DFAny.Type, LN, V](implicit tokenOf : DFAny.Token.ToFit.Summon[LT, V, TokenT[LT#TToken, LT]], sameLength : `LN == RN`.CheckedShell[LN, Int]) : ToFit[LT, LN, V] = super.__DFVectorTokenVectorToFit
        final override implicit def __DFVectorTokenVectorAsIs[LT <: DFAny.Type, LN, V](implicit tokenOf : DFAny.Token.AsIs.Summon[LT, V, TokenT[LT#TToken, LT]], sameLength : `LN == RN`.CheckedShell[LN, Int]) : AsIs[LT, LN, V] = super.__DFVectorTokenVectorAsIs
      }
    }
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////

  object Pattern extends PatternCO {
    //No pattern is acceptable for DFVector
  }

  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Op
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  object Op {
    sealed trait Frontend {
      protected implicit class __DFVectorOps[LT <: DFAny.Type, LN](val left : DFVector[LT, LN]){
        def === [RN](right : DFVector[LT, RN])(implicit op: DFAny.`Op==`.Builder[DFVector[LT, LN], DFVector[LT, RN]]) = op(left, right)
        def =!= [RN](right : DFVector[LT, RN])(implicit op: DFAny.`Op!=`.Builder[DFVector[LT, LN], DFVector[LT, RN]]) = op(left, right)
//        def ++  [RN](right : DFVector[LT, RN])(implicit op: `Op++`.Builder[DFVector[LT, LN], DFVector[LT, RN]]) = op(left, right)
      }

      protected implicit class __DFVectorAliases[LT <: DFAny.Type, LN, Mod <: DFAny.Modifier.Val](val left : DFAny.Value[Type[LT, LN], Mod]) {
        def apply[I, W](idx : Exact[I])(
          implicit ctx : DFAny.Context, w : BitsWidthOf.IntAux[LN-1, W], op : DFAny.`Op:=,<>`.Builder[DFUInt.Type[W], I]
        ) : DFAny.Value[LT, Mod] = trydf{DFAny.ApplySel.fromArray(left, op(DFUInt.Type(w(left.dfType.cellNum-1)), idx))}
      }
    }
    object Frontend {
      trait Inherited extends Frontend {
        final override protected implicit def __DFVectorAliases[LT <: DFAny.Type, LN, Mod <: Modifier.Val](left : DFAny.Value[Type[LT, LN], Mod]) : __DFVectorAliases[LT, LN, Mod] = super.__DFVectorAliases(left)
        final override protected implicit def __DFVectorOps[LT <: DFAny.Type, LN](left : DFVector[LT, LN]) : __DFVectorOps[LT, LN] = super.__DFVectorOps(left)
      }
      trait Imported extends Frontend {
        final override implicit def __DFVectorAliases[LT <: DFAny.Type, LN, Mod <: Modifier.Val](left : DFAny.Value[Type[LT, LN], Mod]) : __DFVectorAliases[LT, LN, Mod] = super.__DFVectorAliases(left)
        final override implicit def __DFVectorOps[LT <: DFAny.Type, LN](left : DFVector[LT, LN]) : __DFVectorOps[LT, LN] = super.__DFVectorOps(left)
      }
    }
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Assign & Connect
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  object `Op:=,<>` {
    import DFAny.`Op:=,<>`.Builder
    sealed trait Frontend {
      protected implicit def __DFVector_op_DFVector[T <: DFAny.Type, LN, RN](
        implicit
        ctx : DFAny.Context,
        checkLNvRN : `LN == RN`.CheckedShell[LN, RN]
      ) : Builder[Type[T, LN], DFVector[T, RN]] = (left, right) => {
        assert(left.cellType == right.dfType.cellType)
        checkLNvRN.unsafeCheck(left.cellNum, right.dfType.cellNum)
        right.asInstanceOf[DFAny.Of[Type[T, LN]]]
      }

      protected implicit def __DFVector_eq_Capable[T <: DFAny.Type, LN, RN](
        implicit
        checkLNvRN : `LN == RN`.CheckedShell[LN, RN]
      ) : DFAny.`Op==,!=`.Capable[Type[T, LN], Type[T, RN]] = (left, right) => {
        assert(left.cellType == right.cellType)
        checkLNvRN.unsafeCheck(left.cellNum, right.dfType.cellNum)
      }

    }
    object Frontend {
      trait Inherited extends Frontend {
        final override protected implicit def __DFVector_op_DFVector[T <: DFAny.Type, LN, RN](implicit ctx : DFAny.Context, checkLNvRN : `LN == RN`.CheckedShell[LN, RN]) : Builder[Type[T, LN], DFVector[T, RN]] = super.__DFVector_op_DFVector
        final override protected implicit def __DFVector_eq_Capable[T <: DFAny.Type, LN, RN](implicit checkLNvRN : `LN == RN`.CheckedShell[LN, RN]) : `Op==,!=`.Capable[Type[T, LN], Type[T, RN]] = super.__DFVector_eq_Capable
      }
      trait Imported extends Frontend {
        final override implicit def __DFVector_op_DFVector[T <: DFAny.Type, LN, RN](implicit ctx : DFAny.Context, checkLNvRN : `LN == RN`.CheckedShell[LN, RN]) : Builder[Type[T, LN], DFVector[T, RN]] = super.__DFVector_op_DFVector
        final override implicit def __DFVector_eq_Capable[T <: DFAny.Type, LN, RN](implicit checkLNvRN : `LN == RN`.CheckedShell[LN, RN]) : `Op==,!=`.Capable[Type[T, LN], Type[T, RN]] = super.__DFVector_eq_Capable
      }
    }
  }
  object `LN == RN` extends Checked1Param.Int {
    type Cond[LN, RN] = LN == RN
    type Msg[LN, RN] = "This operation does not permit applying different element numbers. Found: LHS-width = "+ ToString[LN] + " and RHS-width = " + ToString[RN]
    type ParamFace = Int
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Concatenation operation
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  object `Op++` {
    @scala.annotation.implicitNotFound("Dataflow variable ${L} does not support a Concatenation Op with the type ${R}")
    trait Builder[-L, -R] extends DFAny.Op.Builder[L, R]

    object Builder {
      type Aux[L, R, Comp0] = Builder[L, R] {
        type Out = Comp0
      }

      object Inference {
        type CalcL[LN, RN] = LN + RN
        type ON[LN, RN, ResN] = TwoFace.Int.Shell2Aux[CalcL, LN, Int, RN, Int, ResN]
      }

      implicit def evDFBits_op_DFBits[T <: DFAny.Type, LN, RN, ON](
        implicit
        ctx : DFAny.Context,
        oN : Inference.ON[LN, RN, ON],
      ) : Builder.Aux[DFVector[T, LN], DFVector[T, RN], DFVector[T, ON]] = new Builder[DFVector[T, LN], DFVector[T, RN]] {
        type Out = DFVector[T, ON]
        def apply(left : DFVector[T, LN], right : DFVector[T, RN]) : Out = {
          // Constructing op
          assert(left.dfType.cellType == right.dfType.cellType)
          val oCellNum = oN(left.dfType.cellNum, right.dfType.cellNum)
          val out = DFAny.Func2(Type(left.dfType.cellType, oCellNum), left, DFAny.Func2.Op.++, right)(_ ++ _)
          out
        }
      }
    }
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////

}
