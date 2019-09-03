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

import DFiant.targetlib.TargetLib
import DFiant.compiler.Backend
import DFiant.internals._
import scala.collection.immutable
import scala.annotation.implicitNotFound

abstract class DFDesign(implicit ctx : DFDesign.Context) extends DFBlock with DFInterface {self =>
  protected[DFiant] trait __DevDFDesign extends __DevDFBlock with __DevDFInterface {
    /////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Conditional Block
    /////////////////////////////////////////////////////////////////////////////////////////////////////////
    //The block by value object is created within the context of the current DFDesign,
    //so we mutate `__theOwnerToBe` via mutableOwner which is passed to the IfBlock constructs
    private[DFiant] def injectConditionalBlock[IB <: DFDesign](ifBlock : IB, block: => Unit)(mutableOwner: MutableOwner) : IB = {
      val originalOwner = mutableOwner.value
      mutableOwner.value = ifBlock
      block
      mutableOwner.value = originalOwner
      ifBlock
    }

    /////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Naming
    /////////////////////////////////////////////////////////////////////////////////////////////////////////
    protected def designType : String = typeName
    private var _designDB : Option[DFDesign.DB] = None
    protected[DFDesign] def designDB : DFDesign.DB = if (isTop) _designDB.get else topDsn.__dev.designDB
    private[DFiant] def constructCodeString : String = designDB.addOwnerBody(designType, bodyCodeString, self)

    final private[DFiant] def valCodeString : String = s"\nval $name = new $constructCodeString {}"
    //  final override def refCodeString(implicit callOwner: DSLOwnerConstruct): String = super.refCodeString

    override def codeString: String = {
      _designDB = Some(new DFDesign.DB)
      val valCode = valCodeString
      if (isTop) s"$designDB\n$valCode" else valCode
    }

    /////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Connections
    /////////////////////////////////////////////////////////////////////////////////////////////////////////
    private def throwConnectionError(toVar : DFAny, fromVal : DFAny, msg : String) =
      throw new IllegalArgumentException(s"\n$msg\nAttempted connection: ${toVar.fullName} <> ${fromVal.fullName} at ${ctx.owner.fullName}")

    final val connectionsTo = CacheDerivedHashMapRO(addedMembers)(Map[DFAny, Source]()) {
      case (hm, c : DFNet.Connection) =>
        var bitH : Int = c.toVal.width-1
        val versionedSource = c.fromVal.source.versioned
        val cons = c.toVal.source.elements.collect {
          case SourceElement(relBitHigh, relBitLow, reverseBits, Some(t)) =>
            val relWidth = relBitHigh - relBitLow + 1
            val bitL = bitH-relWidth+1
            val partial = versionedSource.bitsHL(bitH, bitL).reverse(reverseBits)
            val current = hm.getOrElse(t.dfVal, Source.none(t.dfVal.width))
            if (current.nonEmptyAtHL(relBitHigh, relBitLow))
              throwConnectionError(c.toVal, c.fromVal, s"Target ${c.toVal.fullName} already has a connection: $current")
            val full = current.replaceHL(relBitHigh, relBitLow, partial)
            bitH = bitH-relWidth
            t.dfVal -> full
        }
        hm ++ cons
      case (hm, _) => hm
    }

    final val connectionsFrom = CacheDerivedHashMapRO(addedMembers)(Map[DFAny, List[Source]]()) {
      case (hm, c : DFNet.Connection) =>
        var bitH : Int = c.fromVal.width-1
        val cons = c.fromVal.source.elements.collect {
          case SourceElement(relBitHigh, relBitLow, reverseBits, Some(t)) =>
            val relWidth = relBitHigh - relBitLow + 1
            val bitL = bitH-relWidth+1
            val partial = c.toVal.source.bitsHL(bitH, bitL).reverse(reverseBits)
            val current = hm.getOrElse(t.dfVal, List(Source.none(t.dfVal.width)))
            val full = current match {
              case x :+ xs if (xs.nonEmptyAtHL(relBitHigh, relBitLow)) =>
                current :+ Source.none(t.dfVal.width).replaceHL(relBitHigh, relBitLow, partial)
              case x :+ xs =>
                x :+ xs.replaceHL(relBitHigh, relBitLow, partial)
            }
            bitH = bitH-relWidth
            t.dfVal -> full
        }
        hm ++ cons
      case (hm, _) => hm
    }


    /////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Transparent Ports
    /////////////////////////////////////////////////////////////////////////////////////////////////////////
    private def addTransparentPorts(cls : Class[_]) : List[(DFAny, DFAny.Port[DFAny, DFDir])] =
      if (cls == null || cls == classOf[DFDesign] ||
        classOf[ConditionalBlock].isAssignableFrom(cls) || classOf[DFFunc2[_,_,_]].isAssignableFrom(cls)) List()
      else {
        val fields = cls.getDeclaredFields.toList
        fields.flatMap{f =>
          f.setAccessible(true)
          val ref = f.get(self)
          ref match {
            case ref : DFAny if (ref ne null) && (ref.owner ne self) =>
              nameFirst = true //Arguments may lead to new names, so we name the owner first
              val dir = if (f.getType.isAssignableFrom(classOf[DFAny.Connectable[_]])) OUT else IN
              val port = ref.copyAsNewPort(dir).setName(f.getName).asInstanceOf[DFAny.Port[DFAny, DFDir]]
              dir match {
                case d : IN  => port.connectFrom(ref)
                case d : OUT => ref.asInstanceOf[DFAny.Connectable[_]].connectFrom(port)
              }
              Some((ref, port))
            case _ => None
          }
        } ++ addTransparentPorts(cls.getSuperclass)
      }

    lazy val transparentPorts : Map[DFAny, DFAny.Port[DFAny, DFDir]] = addTransparentPorts(self.getClass).toMap
  }
  override private[DFiant] lazy val __dev : __DevDFDesign = new __DevDFDesign {}
  import __dev._

  final override implicit def __theOwnerToBe : DFDesign = mutableOwner.value.asInstanceOf[DFDesign]

  protected def atOwnerDo[T](block : => T) : T = {
    val originalOwner = mutableOwner.value
    mutableOwner.value = owner.asInstanceOf[DFBlock]
    val ret = block
    mutableOwner.value = originalOwner
    ret
  }

  final lazy val isTop : Boolean = __dev.isTop

  private def openInputsCheck() : Unit = discoveredMembers.collect {
    case p : DFAny.Port[_,_] if p.dir.isIn && !isTop && !p.isConnected && p.initLB.get.isEmpty =>
      throw new IllegalArgumentException(s"\nFound an uninitialized open input port: ${p.fullName}")
  }
  private lazy val init : Unit = {
    openInputsCheck()
  }
  final def printCodeString : this.type = {println(codeString); this}
  def compileToVHDL : Backend.VHDL = new Backend.VHDL(this)
  final def printVHDLString : this.type = {compileToVHDL.print(); this}
  transparentPorts //force transparent ports to be added as regular ports before all other members
  if (!self.isInstanceOf[ConditionalBlock]) id
}

object DFDesign {
  protected[DFiant] sealed trait AllowTOP
  object allowTop {
    implicit object __AllowTop extends AllowTOP
  }
  implicit def fetchDev(from : DFDesign)(implicit devAccess: DFiant.dev.Access) : from.__dev.type = from.__dev
  private[DFiant] type Context = DFBlock.Context
  @implicitNotFound(errors.MissingContext.msg)
  trait ContextOf[+T] extends DSLContext {
    val ownerOption : Option[DFBlock]
    val targetLib: TargetLib
    val config : DFAnyConfiguration
    val meta : Meta
  }
  object ContextOf {
    implicit def ev[T](
      implicit
      evOwner : DFBlock,
      evBasicLib : TargetLib,
      evConfig : DFAnyConfiguration,
      evMeta : Meta,
      forceNotVar : Meta.ForceNotVar[ContextOf[_]]
    ) : ContextOf[T] = new ContextOf[T] {
      val ownerOption : Option[DFBlock] = Some(evOwner)
      val targetLib: TargetLib = evBasicLib
      val config: DFAnyConfiguration = evConfig
      val meta: Meta = evMeta
    }
    implicit def evTop[T](
      implicit
      evAllowTop : AllowTOP, //Must have an implicit AllowTOP in scope
      evBasicLib : TargetLib,
      evConfig : DFAnyConfiguration,
      evMeta : Meta,
      forceNotVar : Meta.ForceNotVar[ContextOf[_]]
    ) : ContextOf[T] = new ContextOf[T] {
      val ownerOption : Option[DFBlock] = None
      val targetLib: TargetLib = evBasicLib
      val config: DFAnyConfiguration = evConfig
      val meta: Meta = evMeta
    }
  }
  private[DFiant] class DB extends DSLOwnerConstruct.DB[DFDesign, String] {
    def ownerToString(designTypeName : String, designBodyString : String) : String =
      s"\ntrait $designTypeName extends DFDesign {$designBodyString\n}"
  }

  trait Gen[DFD <: DFDesign] {
    def apply()(implicit ctx : DFBlock.Context) : DFD
  }
  object Gen {
    import scala.reflect.macros.blackbox
    import scala.language.experimental.macros
    import allowTop.__AllowTop

    def helper[DFD <: DFDesign : c.WeakTypeTag](c : blackbox.Context) : c.Tree = {
      import c.universe._
      val tpe = weakTypeOf[DFD]
      val sym = symbolOf[DFD]
      val genTree =
        q"""
            new DFiant.DFDesign.Gen[$tpe]{
              def apply()(implicit n : DFiant.DFBlock.Context) : $tpe = new $sym {}
            }
          """
      genTree
    }
    implicit def apply[DFD <: DFDesign] : Gen[DFD] = macro helper[DFD]
  }

}




