/*
 *     This file is part of DFiant.
 *
 *     DFiant is free software: you can redistribute it and/or modify
 *     it under the terms of the GNU Lesser General Public License as published by
 *     the Free Software Foundation, either version 3 of the License, or
 *     any later version.
 *
 *     DFiant is distributed in the hope that it will be useful,
 *     but WITHOUT ANY WARRANTY; without even the implied warranty of
 *     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *     GNU Lesser General Public License for more details.
 *
 *     You should have received a copy of the GNU Lesser General Public License
 *     along with DFiant.  If not, see <https://www.gnu.org/licenses/>.
 */

package DFiant

import DFiant.internals._
import singleton.twoface._

abstract class DFFunc2[Comp <: DFFunc2[Comp, L, R], L <: DFAny, R <: DFAny]
(L : L, val opString : String, R : R)(_width : Int) (
  implicit ctx: DFComponent.Context[Comp], cmp: DFAny.Companion
) extends DFComponent[Comp] with DSLSelfConnectedFoldableOwnerConstruct with CanBePiped {self : Comp =>
  protected[DFiant] trait __DevFunc2Comp extends __DevDFComponent with __DevDFAny {
    lazy val leftArg : L = L.replacement().asInstanceOf[L]
    lazy val rightArg : R = R.replacement().asInstanceOf[R]
    /////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Naming
    /////////////////////////////////////////////////////////////////////////////////////////////////////////
    override def refCodeString(implicit callOwner: DSLOwnerConstruct): String =
      if (isFolded) super.refCodeString else outResult.refCodeString(ctx.owner)
    override def constructCodeStringDefault: String = foldedConstructCodeString
    override protected def designType : String = s"`Func2Comp$opString`"
    override def foldedConstructCodeString: String = s"${leftBalancedSource.refCodeString} $opString ${rightBalancedSource.refCodeString}"
    override def codeString: String = if (isFolded) super.codeString else valCodeString

    /////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Member discovery
    /////////////////////////////////////////////////////////////////////////////////////////////////////////
    override val isAssignable: Boolean = false
//    override def discoveryDependencies: List[DFAnyMember] = super.discoveryDependencies :+ outResult :+ leftArg :+ rightArg

    override def inletSourceLB : LazyBox[Source] = {
      LazyBox.Args1[Source, Option[Int]](self)(l => Source.withLatency(self, l)/*.pipe(extraPipe)*/, maxLatency)
    }

    override lazy val initCB: CacheBoxRO[Seq[TToken]] = initOf(self)
    private[DFiant] var usedAsWide = false //TODO: remove this hack

    final lazy val constLB : LazyBox[TToken] = LazyBox.Args2(self)(tokenFunc, inLeft.constLB.asInstanceOf[LazyBox[leftArg.TToken]], inRight.constLB.asInstanceOf[LazyBox[rightArg.TToken]])
    //  private var extraPipe : Int = 0
    //  def pipe() : this.type = pipe(1)
    //  private[DFiant] override def pipeGet : Int = extraPipe
    //  final def pipe(p : Int) : this.type = {extraPipe = p; this}
    final lazy val leftBalancedSource = leftArg.thisSourceLB.get.balanceTo(maxLatency.get)
    final lazy val rightBalancedSource = rightArg.thisSourceLB.get.balanceTo(maxLatency.get)

    final val leftLatency = LazyBox.Args1[Option[Int], Source](self)(s => s.getMaxLatency, leftArg.thisSourceLB)
    final val rightLatency = LazyBox.Args1[Option[Int], Source](self)(s => s.getMaxLatency, rightArg.thisSourceLB)
    final lazy val maxLatency = LazyBox.Args2[Option[Int], Option[Int], Option[Int]](self)((l, r) => List(l, r).max, leftLatency, rightLatency)
  }
  override private[DFiant] lazy val __dev : __DevFunc2Comp = new __DevFunc2Comp {}
  __dev
  import __dev._

  final val width : TwoFace.Int[Width] = TwoFace.Int.create[Width](_width)
  protected val tokenFunc : (L#TToken, R#TToken) => TToken
  override protected val blackBoxFunctions : Map[DFAny, BlackBoxFunction[_]] = Map(self -> BlackBoxFunction(self)(leftArg, rightArg)(tokenFunc))

  final val inLeft = leftArg.copyAsNewPort(IN)
  final val inRight = rightArg.copyAsNewPort(IN)
  final val outResult = this.copyAsNewPort(OUT)

  atOwnerDo {
    inLeft.connectWith(leftArg)
    inRight.connectWith(rightArg)
//    outResult.connectVal2Port(this)
  }
}
object DFFunc2 {
  implicit def fetchDev(from : DFFunc2[_,_,_])(implicit devAccess: DevAccess) : from.__dev.type = from.__dev
}

trait CompAlias extends CanBePiped {
  val comp : DFFunc2[_,_,_]
  lazy val unextendedLeft : DFAny = comp.leftArg.asInstanceOf[DFAny]
  final val alias = this.asInstanceOf[DFAny.Alias[_]]
//  val bypassAlias : Boolean
//  def pipe() : this.type = pipe(1)
//  private[DFiant] override def pipeGet : Int = comp.pipeGet
//  def pipe(p : Int) : this.type = {comp.pipe(p); this}
}