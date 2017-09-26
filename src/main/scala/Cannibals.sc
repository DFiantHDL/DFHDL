/////////////////////////////////////////////////////////////////////////////////////
// Any cannibal traits and abstract classes
/////////////////////////////////////////////////////////////////////////////////////
trait AnyCannibalLike {
  type TBasic <: AnyCannibalLike
  type THungry <: AnyHungryCannibalA with TBasic
  type TChild <: TBasic
  //name of the cannibal
  protected val _name : String
  def getName() = _name

  //Any cannibal can play with any cannibal
  def playWith(anyCannibal : AnyCannibalLike) : Unit

  //Any cannibal can bring a child of the same tribe into the world.
  //If the cannibal is normal then the child will be normal
  //If the cannibal is hungry then the child will be hungry
  def birthChild(childName : String) : TChild
  protected final def completeNameOfChild(childName : String) = childName + " (child of " + getName() + ")"
}

abstract class AnyNormalCannibalA(name : String) extends AnyCannibalLike {
  type TChild = TBasic
  protected val _name : String = name
  def playWith(anyCannibal : AnyCannibalLike) : Unit = {
    println(getName() + " playing with " + anyCannibal.getName())
  }
}
abstract class AnyHungryCannibalA(name : String, eatingTool : String) extends AnyCannibalLike {
  type TChild = THungry
  protected val _name : String = name
  def playWith(anyCannibal : AnyCannibalLike) : Unit = {
    println(getName() + " playing hungrily with " + anyCannibal.getName())
  }
  def eat(sameCannibal : TBasic) : Unit = {
    println(getName() + " eating " + sameCannibal.getName() + " using " + eatingTool)
  }
}
/////////////////////////////////////////////////////////////////////////////////////


/////////////////////////////////////////////////////////////////////////////////////
// Jungle cannibal traits and case classes
/////////////////////////////////////////////////////////////////////////////////////
trait JungleCannibal extends AnyCannibalLike {
  type TBasic = JungleCannibal
  type THungry = JungleCannibal.JungleHungryCannibal
  //Jungle cannibals can hunt with other jungle cannibals
  def huntWith(jungleCannibal : JungleCannibal) : Unit = {
    println(getName() + " hunting with " + jungleCannibal.getName())
  }
}
object JungleCannibal {
  case class JungleNormalCannibal(name : String) extends AnyNormalCannibalA(name) with JungleCannibal {
    def birthChild(childName : String) = JungleNormalCannibal(completeNameOfChild(childName))
  }

  case class JungleHungryCannibal(name : String) extends AnyHungryCannibalA(name,"hands") with JungleCannibal {
    def birthChild(childName : String) = JungleHungryCannibal(completeNameOfChild(childName))

    def huntAndEat(jungleCannibal : JungleCannibal) : Unit = {
      huntWith(jungleCannibal)
      eat(jungleCannibal)
    }
  }

  def apply(name: String) = JungleHungryCannibal(name)
}
/////////////////////////////////////////////////////////////////////////////////////


/////////////////////////////////////////////////////////////////////////////////////
// Urban cannibal traits and case classes
/////////////////////////////////////////////////////////////////////////////////////
trait UrbanCannibal extends AnyCannibalLike {
  type TBasic = UrbanCannibal
  type THungry = UrbanCannibal.UrbanHungryCannibal
  //Urban cannibals can read with other urban cannibals
  def readWith(urbanCannibal : UrbanCannibal) : Unit = {
    println(getName() + " reading with " + urbanCannibal.getName())
  }
}
object UrbanCannibal {
  case class UrbanNormalCannibal (name : String) extends AnyNormalCannibalA(name) with UrbanCannibal {
    def birthChild(childName : String) = UrbanNormalCannibal(completeNameOfChild(childName))
  }

  case class UrbanHungryCannibal (name : String) extends AnyHungryCannibalA(name,"fork") with UrbanCannibal {
    def birthChild(childName : String) = UrbanHungryCannibal(completeNameOfChild(childName))

    //Urban cannibals can read with and then eat other urban cannibals
    def readAndEat(urbanCannibal: UrbanCannibal) : Unit = {
      readWith(urbanCannibal)
      eat(urbanCannibal)
    }
  }

  def apply(name: String) = UrbanHungryCannibal(name)
}
/////////////////////////////////////////////////////////////////////////////////////

/////////////////////////////////////////////////////////////////////////////////////
// Adopting cannibal traits and case classes
/////////////////////////////////////////////////////////////////////////////////////
trait AdoptingCannibalLike extends AnyCannibalLike {
  type T1 <: AnyCannibalLike
  type T2 <: AnyCannibalLike
  type T3 <: AnyCannibalLike
  type TAdopt1 <: T1#TBasic
  type TAdopt2 <: T2#TBasic
  type TAdopt3 <: T3#TBasic
  //Adopting cannibals can adopt any hungry cannibal.
  //If the adopter is hungry then the returned adopted should keep its type as is.
  //If the adopter is not hungry then the returned adopted is the same type but not hungry.
  def adopt(sameCannibal : T1#THungry) : TAdopt1 = sameCannibal.asInstanceOf[TAdopt1]
  def adopt(sameCannibal : T2#THungry)(implicit d1: DummyImplicit) : TAdopt2 = sameCannibal.asInstanceOf[TAdopt2]
  def adopt(sameCannibal : T3#THungry)(implicit d1: DummyImplicit, d2: DummyImplicit) : TAdopt3 = sameCannibal.asInstanceOf[TAdopt3]
}

abstract class AdoptingNormalCannibalA(name : String) extends AnyNormalCannibalA(name) with AdoptingCannibalLike {
  type TAdopt1 = T1#TBasic
  type TAdopt2 = T2#TBasic
  type TAdopt3 = T3#TBasic
}

abstract class AdoptingHungryCannibalA(name : String) extends AnyHungryCannibalA(name, "my kids") with AdoptingCannibalLike {
  type TAdopt1 = T1#THungry
  type TAdopt2 = T2#THungry
  type TAdopt3 = T3#THungry
}
///////////////////////////////////////////////////////////////////////////////////////

trait OronTrait extends AdoptingCannibalLike {
  type T1 = JungleCannibal
  type T2 = UrbanCannibal
  type TBasic = OronTrait
  type THungry = OronHungry

  val a = adopt(JungleCannibal("oronA"))
  val b = adopt(UrbanCannibal("oronB"))
}

case class OronNormal(name : String) extends AdoptingNormalCannibalA(name) with OronTrait {
  def birthChild(childName : String) = OronNormal(completeNameOfChild(childName))
}
case class OronHungry(name : String) extends AdoptingHungryCannibalA(name) with OronTrait {
  def birthChild(childName : String) = OronHungry(completeNameOfChild(childName))
  a.eat(a)
}
val oh = OronHungry("oh")
val on = OronNormal("on")
oh.playWith(oh)
oh.playWith(on)
oh.playWith(on.a)
//oh.a.eat(oh)
oh.a.eat(oh.a)
//o.a.eat(o)
val jnc = JungleCannibal.JungleNormalCannibal("jnc")
val jhc = JungleCannibal("jhc")
val unc = UrbanCannibal.UrbanNormalCannibal("unc")
val uhc = UrbanCannibal("uhc")
val cjnc = jnc.birthChild("cjnc")
val cjhc = jhc.birthChild("cjhc")
//Must execute
jnc.playWith(jnc)
jnc.playWith(unc)
jhc.eat(jnc)
uhc.eat(unc)
jnc.huntWith(jhc)
jhc.huntWith(jnc)
cjnc.huntWith(jhc)
cjhc.huntAndEat(jnc)
//jnc.huntWith(unc)
//jnc.eat(jnc)
//cjnc.huntWith(unc)
//cjnc.eat(jnc)