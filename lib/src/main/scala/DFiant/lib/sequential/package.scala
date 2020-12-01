package DFiant
package lib

import DFiant.internals.Exact
import DFDesign.Frontend._

/**
  * Provides various Pseudo-Sequential defintions that easily construct FSMs.
  */
package object sequential {

  @df def doWhile[C](cond: => Exact[C])(block: => Unit)(implicit arg: DFBool.Arg[C]) : FSM = FSM {
    ifdf(arg(cond)) {
      block
    }.elsedf {
      nextStep.goto()
    }
  }

  @df def waitWhile[C](cond: => Exact[C])(implicit arg: DFBool.Arg[C]): FSM =
    doWhile(cond){}(arg, ctx, ta)

  @df def doUntil[C](cond: => Exact[C])(block: => Unit)(implicit arg: DFBool.Arg[C]): FSM = FSM {
    block
    ifdf(arg(cond)) {
      nextStep.goto()
    }
  }

  @df def doOnce(block: => Unit): FSM = FSM {
    val once = DFBool() init true
    ifdf(once)(block)
    once := false
  }

  @df def waitUntil[C](cond: => Exact[C])(implicit arg: DFBool.Arg[C]): FSM =
    doUntil(cond){}(arg, ctx, ta)

  @df def waitForever() : FSM = FSM {}

  @df private def privDoFor(range : Range, guard : Option[DFBool] = None)(block : DFUInt[Int] => Unit) : FSM = {
    assert(range.head >= 0 && range.last >= 0, "\ndoFor currently does not support negative range values")
    assert(range.nonEmpty, "\ndoFor cannot accept a null range")
    range match {
      case inclusive : Range.Inclusive =>
        assert(range.last == range.end, s"\nThe last doFor value ${range.last} does not match the inclusive maximum value ${range.end}")
      case exclusive : Range.Exclusive =>
        assert(range.last == range.end-1, s"\nThe last doFor value ${range.last} does not match the exclusive supremum value ${range.end}")
    }
    FSM {
      val forCnt = DFUInt.max(range.head max range.last) init range.head
      block(forCnt)
      def cntBlock = {
        ifdf (forCnt === range.last) {
          forCnt := range.head
          nextStep.goto()
        }.elsedf {
          forCnt := forCnt + range.step
        }
      }
      guard match {
        case Some(cond) => ifdf(cond){cntBlock}
        case None => cntBlock
      }
    }
  }
  @df def doFor(range : Range)(block : DFUInt[Int] => Unit) : FSM =
    privDoFor(range, None)(block)
  @df def doFor[G](range : Range, guard : Exact[G])(block : DFUInt[Int] => Unit)(implicit arg: DFBool.Arg[G]) : FSM =
    privDoFor(range, Some(arg(guard)))(block)

}
