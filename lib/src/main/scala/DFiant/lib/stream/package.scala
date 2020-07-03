package DFiant.lib

import DFiant._
import internals._
import DFDesign.Implicits._
package object stream {
//  implicit class ValToStream[Type <: DFAny.Type](val left : DFAny.Value[Type, DFAny.Modifier.NewVar] with DFAny.Dcl.Uninitialized) {
//    def <> (streamDir: StreamDir)(implicit ctx : DFDesign.Context) : DFStream[Type] = ???
//    def <> [D <: PortDir](dir : D)(implicit ctx : DFAny.Context) : DFAny.Value[Type, DFAny.Modifier.Port[PortDir]] with DFAny.Dcl.Uninitialized =
//      new DFAny.NewVarOps[Type](left) <> dir
//  }
//  implicit class StreamExt[Type <: DFAny.Type](val left : DFStream[Type]) {
//    def init(that : left.payload.dfType.InitAble[DFAny.Of[Type]]*)(
//      implicit op : left.payload.dfType.InitBuilder[DFAny.Of[Type]], ctx : DFAny.Context
//    ) : DFStream[Type] = ???
//  }

  implicit class StreamExt[Type <: DFAny.Type](left : DFAny.Of[Type]) {
    @df def emptydf : DFAny.Of[Type] = left.asNewVar

    @df def dropdf(n : Int) : DFAny.Of[Type] = {
      if (n <= 0) left
      else {
        val ret = left.asNewVar
        val cnt = DFUInt.max(n) init 0
        ifdf(cnt === n) {
          ret := left
        }.elsedf {
          ret.dontProduce()
          cnt := cnt + 1
        }
        ret
      }
    }

    @df def dropWhiledf(p : DFAny.Of[Type] => DFBool) : DFAny.Of[Type] = {
      val ret = left.asNewVar
      val stop = DFBool() init false
      ifdf(!stop && p(left)) {
        ret := left
      }.elsedf {
        stop := true
        ret.dontProduce()
      }
      ret
    }

    @df def takedf(n : Int) : DFAny.Of[Type] = {
      val ret = left.asNewVar
      if (n <= 0) ret
      else {
        val cnt = DFUInt.max(n) init 0
        ifdf(cnt === n) {
          ret.dontProduce()
        }.elsedf {
          ret := left
          cnt := cnt + 1
        }
        ret
      }
    }

    @df def takeWhiledf(p : DFAny.Of[Type] => DFBool) : DFAny.Of[Type] = {
      val ret = left.asNewVar
      val stop = DFBool() init false
      ifdf(stop || !p(left)) {
        stop := true
        ret.dontProduce()
      }.elsedf {
        ret := left
      }
      ret
    }
  }
}
