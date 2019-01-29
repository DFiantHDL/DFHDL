package singleton.ops

import singleton.twoface._

/**
  * Created by soronpo on 6/29/17.
  */

object Bla {
  object BitIndex extends Checked1Param.Int {
    type Cond[I, W] = (I < W) && (I >= W.`0`.T)
    type Msg[I, W] = W.`"Bit index "`.T + ToString[I] + W.`" is out of range of width "`.T + ToString[W]
    type ParamFace = Int
  }

//  val a = implicitly[BitIndex.Checked[W.`1`.T, Int]]
  def foo[T](f : BitIndex.Checked[T, W.`5`.T]) : Unit = {}
  var one = 1
  foo(one)


}
