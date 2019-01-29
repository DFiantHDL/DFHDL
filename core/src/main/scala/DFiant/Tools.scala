package DFiant


import scala.language.experimental.macros

import scala.concurrent.duration.FiniteDuration
import scala.reflect.macros.blackbox

object printCompileTime {
  def apply(code: String): FiniteDuration = macro CompileTimeMacros.applyImpl
}

//@macrocompat.bundle
class CompileTimeMacros(val c: blackbox.Context) {
  import c.universe._

  def applyImpl(code: Tree): Tree = {
    def wallClock(codeStr: String): Long = {
      try {
        val t1 = System.nanoTime()
        c.typecheck(c.parse(codeStr.stripMargin))
        val t2 = System.nanoTime()
        t2 - t1
      } catch {
        case ex: Exception => c.abort(c.enclosingPosition, ex.getMessage)
      }
    }

    val Literal(Constant(codeStr: String)) = code
    val elapsedTime = wallClock(codeStr)

    print(s"TIME AT ${c.enclosingPosition} IS ${elapsedTime/1000000000}sec")
    q"_root_.scala.concurrent.duration.Duration.fromNanos($elapsedTime)"
  }
}

//package DFiant
//import scala.math.{ceil, log}
//
//
//object Implicits {
//  implicit class RichInt(val value: Int) extends AnyVal {
//    def downto (n: Int) = value to n by -1
//    def downtil (n: Int) = value until n by -1
//    def :: (n : Int) = if (n <= value) n to value else n downto value
//    def ~^ (n : Int) : Int = BigInt(value).pow(n).toInt
//    //def intToZeroOrOne : ZeroOrOne = refineMV[ZeroOrOne](value)
//  }
//
////  implicit class DFCollection[Val <: DFAny, Var <: Val with DFAny.Var[WUnsafe, Val, Var]](val iterable : Iterable[Val]) extends AnyVal {
////    def maxWidth : Int = {
////      var width = 0
////      iterable.foreach(e => width = math.max(width,e.width))
////      width
////    }
////    def maxWidthElem : Val = {
////      var width = 0
////      var elem = iterable.head
////      iterable.foreach(e => {width = math.max(width,e.width); if (e.width == width) elem = e})
////      elem
////    }
////    def toDFValNext : Val = {
////      val dfVar = maxWidthElem.newEmptyDFVar
////      iterable.zipWithIndex.foreach{case (e, i) => dfVar.asInstanceOf[e.TVar].assignNext(i, e.asInstanceOf[e.TVal])}
////      dfVar.asInstanceOf[Val]
////    }
////  }
//
////  implicit class DFBitsCollection(iterable : Iterable[DFBits.Unsafe]){
////    def maxWidth : Int = {
////      var width = 0
////      iterable.foreach(e => width = math.max(width,e.width))
////      width
////    }
////    def toDFValNext(width : Int = maxWidth) : DFBits.Unsafe = {
////      val dfVar = DFBits.Unsafe(width)
////      iterable.zipWithIndex.foreach{case (e, i) => dfVar.assignNext(i, e)}
////      dfVar
////    }
////  }
//
//}
//
//object log2Up {
//  def apply(in: Int): Int = if(in == 1) 1 else ceil(log(in.toDouble)/log(2.toDouble)).toInt
//}
//
