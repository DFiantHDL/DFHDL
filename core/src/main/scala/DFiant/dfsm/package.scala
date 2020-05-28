package DFiant
import internals._
import DFDesign.Implicits._

package object dfsm {
  def step(block : => Unit)(
    implicit ctx : DFBlock.Context
  ) : Step = Step.Basic(() => block)
//  def doWhile[C](cond : => C)(block : => Unit)(
//    implicit arg : DFBool.Arg[0], ctx : DFBlock.Context
//  ) : FSMCond = FSMCond(step(block), () => !arg())
//  def waitWhile[C](cond : => C)(
//    implicit arg : DFBool.Arg[0], ctx : DFBlock.Context
//  ) : FSMCond = doWhile(cond){}
//  def doUntil[C](cond : => C)(block : => Unit)(
//    implicit arg : DFBool.Arg[0], ctx : DFBlock.Context
//  ) : FSMCond = FSMCond(step(block), () => arg())
//  def waitUntil[C](cond : => C)(
//    implicit arg : DFBool.Arg[0], ctx : DFBlock.Context
//  ) : FSMCond = doUntil(cond){}
//  def wait()(implicit ctx : DFBlock.Context) : Step = step({})
//  def doFor(range : Range, guard : Option[DFBool] = None)(block : DFUInt[Int] => Unit)(
//    implicit ctx : DFBlock.Context
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
