/*
 *     This file is part of DFiant.
 *
 *     DFiant is free software: you can redistribute it and/or modify
 *     it under the terms of the Lesser GNU General Public License as published by
 *     the Free Software Foundation, either version 3 of the License, or
 *     any later version.
 *
 *     DFiant is distributed in the hope that it will be useful,
 *     but WITHOUT ANY WARRANTY; without even the implied warranty of
 *     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *     Lesser GNU General Public License for more details.
 *
 *     You should have received a copy of the Lesser GNU General Public License
 *     along with DFiant.  If not, see <https://www.gnu.org/licenses/>.
 */

package DFiant
import internals._

protected trait DFBlackBox extends DFInterface {
  protected[DFiant] trait __DevDFBlackBox extends __DevDFInterface {
    /////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Member discovery
    /////////////////////////////////////////////////////////////////////////////////////////////////////////
    private[DFiant] final def depsOf[DF <: DFAny](dfVal : DF) : Set[DFAnyMember] = {
      val bbf = blackBoxFunctions(dfVal)
      bbf.inputs.toSet
    }

    /////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Initialization
    /////////////////////////////////////////////////////////////////////////////////////////////////////////
    private[DFiant] final def initOf[DF <: DFAny](dfVal : DF) : CacheBoxRO[Seq[dfVal.TToken]] = {
      val bbf = blackBoxFunctions(dfVal)
      val inputInits = bbf.inputs.map(i => i.initCB)

      CacheDerivedRO(inputInits) {
        bbf.init.asInstanceOf[Seq[dfVal.TToken]]
      }
    }

    /////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Constant
    /////////////////////////////////////////////////////////////////////////////////////////////////////////
    private[DFiant] final def constOf[DF <: DFAny](dfVal : DF) : CacheBoxRO[dfVal.TToken] = {
      val bbf = blackBoxFunctions(dfVal)
      val inputConsts = bbf.inputs.map(i => i.constCB)

      CacheDerivedRO(inputConsts) {
        bbf.const.asInstanceOf[dfVal.TToken]
      }
    }
  }
  override private[DFiant] lazy val __dev : __DevDFBlackBox = ???
  import __dev._

  protected abstract class BlackBoxFunction[O <: DFAny] private (val output : O)(val inputs : List[DFAny]) {
    def init : Seq[output.TToken]
    def const : output.TToken
  }
  protected object BlackBoxFunction {
    def apply[O <: DFAny, L <: DFAny, R <: DFAny](o : O)(l : L, r : R)(func : (l.TToken, r.TToken) => o.TToken) =
      new BlackBoxFunction(o)(List(l, r)) {
        def init: Seq[output.TToken] = DFAny.TokenSeq(l.initCB.unbox, r.initCB.unbox)(func).asInstanceOf[Seq[output.TToken]]
        def const: output.TToken = func(l.constCB.unbox, r.constCB.unbox).asInstanceOf[output.TToken]
      }
  }
  protected val blackBoxFunctions : Map[DFAny, BlackBoxFunction[_]] = Map()
}

object DFBlackBox {
  implicit def fetchDev(from : DFBlackBox)(implicit devAccess: DevAccess) : from.__dev.type = from.__dev
}