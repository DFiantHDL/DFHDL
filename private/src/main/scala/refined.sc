///////////////////////////////
////Runtime guard for boolean
///////////////////////////////
//object zero_or_one {
//  import scala.language.implicitConversions
//
//  class ZeroOrOneRuntime private (val value: Int) extends AnyVal
//
//  object ZeroOrOneRuntime {
//    def apply(v: Int) = {
//      require(v == 0 || v == 1, "0 or 1 accepted only")
//      new ZeroOrOneRuntime(v)
//    }
//
//    implicit def toZeroOrOneRuntime(v: Int) = ZeroOrOneRuntime(v)
//  }
//
//  implicit def toInt(nn: ZeroOrOneRuntime) = nn.value
//}
//
//import zero_or_one._
//
//var a : ZeroOrOneRuntime = 0
//val a_bad :ZeroOrOneRuntime = 2
//
//for (i <- 0 to 10)
//  a = i


//////////////////////////////////
//Compile-time guard for boolean
//////////////////////////////////
import eu.timepit.refined._
import eu.timepit.refined.api.Refined
import eu.timepit.refined.auto._
import eu.timepit.refined.numeric._


type ZeroOrOneLiteral = Int Refined Interval.Closed[W.`0`.T, W.`1`.T]

//implicit class castableZeroOrOneLiteral(v: Int) {
//  def cast: ZeroOrOneLiteral = {
//    refineMV[ZeroOrOneLiteral](v)
//  }
//}

var b : ZeroOrOneLiteral = 1

//for (i <- 0 to 10)
//  b = i//.cast

