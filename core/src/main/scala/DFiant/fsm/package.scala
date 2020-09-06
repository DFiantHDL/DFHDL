package DFiant

import internals._
import DFDesign.Implicits._

package object fsm {
  type FSM = FSMMember with FSMMember.Complete
  private[fsm] implicit def funcConv[T](t : => T) : () => T = () => t
  def step(block : => Unit)(
    implicit ctx : DFBlock.Context
  ) : FSMMember.BasicStep[Unit] = new FSMMember.BasicStep[Unit](block).track
  def stepR[R](block : => R)(
    implicit ctx : DFBlock.Context
  ) : FSMMember.BasicStep[R] = new FSMMember.BasicStep[R](block).track

  def doWhile[C](cond : => Exact[C])(block : => Unit)(
    implicit arg : => DFBool.Arg[C], ctx : DFBlock.Context
  ) = {
    import ctx.db.getSet
    step(block) =?> (!arg(cond)).anonymize
  }
  def waitWhile[C](cond : => Exact[C])(
    implicit arg : => DFBool.Arg[C], ctx : DFBlock.Context
  ) = doWhile(cond){}(arg, ctx)
  def doUntil[C](cond : => Exact[C])(block : => Unit)(
    implicit arg : => DFBool.Arg[C], ctx : DFBlock.Context
  ) = step(block) =?> arg(cond)
  def waitUntil[C](cond : => Exact[C])(
    implicit arg : => DFBool.Arg[C], ctx : DFBlock.Context
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
