package DFiant.basiclib

import DFiant.core._

object Reusable {
//  trait ReusableO[B <: BasicOp] {
//    protected val basicOp : B
//    private val opLeft = DFBits.Unsafe(basicOp.widthLeft)
//    private val opRight = DFBits.Unsafe(basicOp.widthRight)
//    private val opOut = basicOp(opLeft, opRight)
//    private var reuseIdx = 0
//    def reuse(left : DFBits.Unsafe, right : DFBits.Unsafe) : DFBits.Unsafe = {
//      opLeft.assignNext(reuseIdx, left)
//      opRight.assignNext(reuseIdx, right)
//      reuseIdx += 1
//      opOut.next(reuseIdx)
//    }
//  }

//  abstract class ReusableOp[B <: BasicOp](reuseNum : Int) {
//    protected val basicOp : B
//    private val leftArr = Array.tabulate(reuseNum)(_ => DFBits.Unsafe(basicOp.widthLeft).dontConsume())
//    private val rightArr = Array.tabulate(reuseNum)(_ => DFBits.Unsafe(basicOp.widthRight).dontConsume())
//    private val outArr = Array.tabulate(reuseNum)(_ => DFBits.Unsafe(basicOp.widthOut).dontProduce())
//    private val reuseCnt = Counter(0 until reuseNum, 0)
//    private var reuseIdx = 0
//    private val opLeft = DFBits.Unsafe(basicOp.widthLeft)
//    private val opRight = DFBits.Unsafe(basicOp.widthRight)
//    private val opOut = basicOp(opLeft, opRight)
//    def reuse(left : DFBits.Unsafe, right : DFBits.Unsafe) : DFBits.Unsafe = {
//      val retVal = outArr(reuseIdx)
//      leftArr(reuseIdx) := left
//      rightArr(reuseIdx) := right
//      reuseIdx += 1
//      retVal
//    }
//    for (i <- 0 until reuseNum) {
//      ifdf (reuseCnt.getValue == i) {
//        opLeft := leftArr(i)
//        opRight := rightArr(i)
//        outArr(i) := opOut
//      }
//    }
//    reuseCnt.step //TODO change to auto increment
//  }

//  def apply[B <: BasicOp](widthLeft : Int, widthRight : Int) = ???
}