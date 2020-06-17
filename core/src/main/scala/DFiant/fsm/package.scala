package DFiant

import internals._
import DFDesign.Implicits._

package object fsm {
  type FSM = FSM.Member with FSM.Complete
  private[fsm] implicit def funcConv[T](t : => T) : () => T = () => t
  def step(block : => Unit)(
    implicit ctx : DFBlock.Context
  ) : FSM.BasicStep[Unit] = new FSM.BasicStep[Unit](block).track
  def stepR[R](block : => R)(
    implicit ctx : DFBlock.Context
  ) : FSM.BasicStep[R] = new FSM.BasicStep[R](block).track

  def doWhile[C](cond : => C)(block : => Unit)(
    implicit arg : => DFBool.Arg[0], ctx : DFBlock.Context
  ) = {
    import ctx.db.getSet
    step(block) =?> (!arg()).anonymize
  }
  def waitWhile[C](cond : => C)(
    implicit arg : => DFBool.Arg[0], ctx : DFBlock.Context
  ) = doWhile(cond){}(arg, ctx)
  def doUntil[C](cond : => C)(block : => Unit)(
    implicit arg : => DFBool.Arg[0], ctx : DFBlock.Context
  ) = step(block) =?> arg()
  def waitUntil[C](cond : => C)(
    implicit arg : => DFBool.Arg[0], ctx : DFBlock.Context
  ) = doUntil(cond){}(arg, ctx)
  def waitForever()(implicit ctx : DFBlock.Context) = step({})

  //  def doFor(range : Range, guard : Option[DFBool] = None)(block : DFUInt[Int] => Unit)(
//    implicit ctx : DFAny.Context
//  ) : FSMCond = {
//    import ctx.db.getSet
//    val width = (range.start max range.end).bitsWidth
//    def cntBlock = {
//      val forCnt = DFUInt(width) init range.start
//      def advanceCnt = forCnt := forCnt + range.step
//      val continueGuard = (forCnt =!= range.last).anonymize
//      ifdf(continueGuard) {
//        block(forCnt)
//        guard match {
//          case Some(cond) => ifdf(cond)(advanceCnt)
//          case None => advanceCnt
//        }
//      }.elsedf {
//        forCnt := range.start
//        ???
//      }
//    }
//    ???
//  }

}
