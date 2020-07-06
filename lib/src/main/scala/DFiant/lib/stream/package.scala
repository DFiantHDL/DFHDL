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

//  implicit class StreamExt[Type <: DFAny.Type](left : DFAny.Of[Type]) {
//    @df def emptydf : DFAny.Of[Type] = left.asNewVar
//
//    @df def dropdf(n : Int) : DFAny.Of[Type] = {
//      if (n <= 0) left
//      else {
//        val ret = left.asNewVar
//        val cnt = DFUInt.max(n) init 0
//        ifdf(cnt === n) {
//          ret := left
//        }.elsedf {
//          ret.dontProduce()
//          cnt := cnt + 1
//        }
//        ret
//      }
//    }
//
//    @df def dropWhiledf(p : DFAny.Of[Type] => DFBool) : DFAny.Of[Type] = {
//      val ret = left.asNewVar
//      val stop = DFBool() init false
//      ifdf(!stop && p(left)) {
//        ret := left
//      }.elsedf {
//        stop := true
//        ret.dontProduce()
//      }
//      ret
//    }
//
//    @df def takedf(n : Int) : DFAny.Of[Type] = {
//      val ret = left.asNewVar
//      if (n <= 0) ret
//      else {
//        val cnt = DFUInt.max(n) init 0
//        ifdf(cnt === n) {
//          ret.dontProduce()
//        }.elsedf {
//          ret := left
//          cnt := cnt + 1
//        }
//        ret
//      }
//    }
//
//    @df def takeWhiledf(p : DFAny.Of[Type] => DFBool) : DFAny.Of[Type] = {
//      val ret = left.asNewVar
//      val stop = DFBool() init false
//      ifdf(stop || !p(left)) {
//        stop := true
//        ret.dontProduce()
//      }.elsedf {
//        ret := left
//      }
//      ret
//    }
//
//    @df def filterdf(keepWhen : DFAny.Of[Type] => DFBool) : DFAny.Of[Type] = {
//      val ret = left.asNewVar
//      ifdf(keepWhen(left)) {
//        ret := left
//      }.elsedf {
//        ret.dontProduce()
//      }
//      ret
//    }
//
//    @df def filterNotdf(discardWhen : DFAny.Of[Type] => DFBool) : DFAny.Of[Type] = {
//      val ret = left.asNewVar
//      ifdf(discardWhen(left)) {
//        ret.dontProduce()
//      }.elsedf {
//        ret := left
//      }
//      ret
//    }
//
//    @df def prependdf(tokens : left.dfType.InitAble[DFAny.Of[Type]]*)(
//      implicit op: left.dfType.InitBuilder[DFAny.Of[Type]]
//    ) : DFAny.Of[Type] = {
//      val varInit = left.asNewVar forcedInit op(left, tokens)
//      val ret = varInit.prev[Int](tokens.length)
//      ret
//    }
//
//    @df def splitdf(num : Int)(
//      implicit op: left.dfType.InitBuilder[DFAny.Of[Type]]
//    ) : List[DFAny.Of[Type]] = {
//      val ret : List[DFAny.VarOf[Type]] = List.tabulate(num)(i => left.asNewVar.setName(s"ret$i"))
//      val sel = DFUInt.until(num) init 0
//      ret.zipWithIndex.foreach{case (r, i) =>
//        ifdf (sel === i){
//          r := left
//        }.elsedf {
//          r.dontProduce()
//        }
//      }
//      ifdf(sel === num) {
//        sel := 0
//      }.elsedf {
//        sel := sel + 1
//      }
//      ret
//    }
//
//    @df def mergedf(right : DFAny.Of[Type]) : DFAny.Of[Type] = {
//      require(left.dfType == right.dfType)
//      val ret = left.asNewVar
//      val sel = DFBool() init 0
//
//      ifdf (sel) {
//        ret := right
//        left.dontConsume()
//      }.elsedf {
//        ret := left
//        right.dontConsume()
//      }
//      sel := !sel
//      ret
//    }
//
//    @df def mergeNonBlockingdf(right : DFAny.Of[Type]) : DFAny.Of[Type] = {
//      require(left.dfType == right.dfType)
//      val ret = left.asNewVar
//      val sel = DFBool() init 0
//
//      ifdf (sel && right.isNotEmpty) {
//        ret := right
//        left.dontConsume()
//      }.elseifdf(!sel && left.isNotEmpty) {
//        ret := left
//        right.dontConsume()
//      }.elsedf {
//        left.dontConsume()
//        right.dontConsume()
//        ret.dontProduce()
//      }
//      sel := !sel
//      ret
//    }
//
//    @df def mergePrioritydf(right : DFAny.Of[Type]) : DFAny.Of[Type] = {
//      require(left.dfType == right.dfType)
//      val ret = left.asNewVar
//
//      ifdf (left.isNotEmpty) {
//        ret := left
//        right.dontConsume()
//      }.elseifdf(right.isNotEmpty) {
//        ret := right
//      }.elsedf {
//        ret.dontProduce()
//      }
//      ret
//    }
//  }

  implicit class StreamCollectionExt[Type <: DFAny.Type](iter : Iterable[DFAny.Of[Type]]) {
    private def checkWidths() : Unit = {
      require(iter.size > 1)
      val dfType = iter.head.dfType
      iter.foreach(i => require(i.dfType == dfType))
    }
    def mergedf(implicit ctx : ContextOf["mergedf"]) : DFAny.Of[Type] = defdf {
      checkWidths()
      val ret = iter.head.asNewVar
      iter.foreach(e => e.dontConsume())
      import fsm._
      val start : FSMMember.Connectable = step{ret := iter.head}
      val sel : FSM = iter.drop(1).foldLeft(start){
        case (s, e) => s ==> step{ret := e}
      } ==> start
      ret
    }
  }
}
