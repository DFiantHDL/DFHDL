// first import all necessary types from package `collection.mutable`
import collection.mutable.{ HashMap, MultiMap, Set, LinkedHashSet }

trait OrderedMultimap[A, B] extends MultiMap[A, B] {
  override def makeSet: Set[B] = new LinkedHashSet[B]
}
// to create a `MultiMap` the easiest way is to mixin it into a normal
// `Map` instance
val mm = new HashMap[String, Set[String]] with OrderedMultimap[String, String]

// to add key-value pairs to a multimap it is important to use
// the method `addBinding` because standard methods like `+` will
// overwrite the complete key-value pair instead of adding the
// value to the existing key
mm.addBinding("oron", "trait DFDesign 123")
mm.addBinding("oron", "trait DFDesign 12")
mm.addBinding("shit", "trait DFDesign 123")

// mm now contains `Map(2 -> Set(b), 1 -> Set(c, a))`

// to check if the multimap contains a value there is method
// `entryExists`, which allows to traverse the including set
mm.get("oron").
