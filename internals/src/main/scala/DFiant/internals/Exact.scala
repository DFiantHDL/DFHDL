package DFiant.internals

//class Exact[T](val value: T)
opaque type Exact[T] = T
object Exact:
//  import scala.reflect.macros.blackbox
//  //For singleton integers we create a special macro that offers some protection from hex literals that
//  //overflow into negative values. E.g., 0x80000000
//  //This is no way close to a full protection from such incidents, but this is enough for most newbie cases
//  //that DFiant code may encounter.
//  implicit def fromIntSing[T <: Int with Singleton](value : T) : Exact[ValueOf[T]] = macro fromIntSingMacro[T]
//  def fromIntSingMacro[T : c.WeakTypeTag](c: blackbox.Context)(value : c.Tree) : c.Tree = {
//    import c.universe._
//    val tpe = weakTypeOf[T]
//    value match {
//      case Literal(Constant(i : Int)) if i < 0 =>
//        val content = new String(value.pos.source.content)
//        val startStr = content.splitAt(value.pos.start)._2
//        if (startStr.startsWith("0x") || startStr.startsWith("0X"))
//          c.abort(c.enclosingPosition, "Found a hex integer literal that overflows into a negative value. \nPlease use DFiant's built in string interpolator literals instead.")
//      case _ => //do nothing
//    }
//    q"new DFiant.internals.Exact[ValueOf[$tpe]](new ValueOf($value))"
//  }
  implicit def fromAnyValSing[T <: Singleton with AnyVal](
      value: T
  ): Exact[ValueOf[T]] = ValueOf(value)
  implicit def fromStringSing[T <: Singleton with String](
      value: T
  ): Exact[ValueOf[T]] = ValueOf(value)
  implicit def fromNonSing[T](value: T)(implicit di: DummyImplicit): Exact[T] =
    value
  implicit def toValueSing[T](precise: Exact[ValueOf[T]]): T =
    precise.value
  implicit def toValue[T](precise: Exact[T]): T = precise
