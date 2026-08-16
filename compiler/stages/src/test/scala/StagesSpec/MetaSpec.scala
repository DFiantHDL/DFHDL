package StagesSpec

import munit.FunSuite
import dfhdl.compiler.ir.Meta
import dfhdl.internals.Position

/** Pins `Meta`'s two comparison notions (`Meta` has no `CanEqual`, so call sites must name one):
  *   - `sameIdentityAs` excludes position and doc, which can drift under a valid elaboration-cache
  *     entry (the code digest hashes typed trees and never sees formatting or comments).
  *     `equals`/`hashCode` implement it, so member value-equality (cache adoption, global
  *     unification) composes through it.
  *   - `sameDclAs` compares all fields; "same declaration" is anchored on position.
  */
class MetaSpec extends FunSuite:
  val posA = Position("FileA.scala", 1, 1, 1, 10)
  val posB = Position("FileB.scala", 5, 3, 7, 2)

  test("identity excludes position and doc") {
    val a = Meta(Some("x"), posA, None, Nil)
    val b = Meta(Some("x"), posB, Some("a doc comment"), Nil)
    assert(a.sameIdentityAs(b))
    assert(a.equals(b))
    assertEquals(a.hashCode, b.hashCode)
  }

  test("identity includes the name") {
    val a = Meta(Some("x"), posA, None, Nil)
    assert(!a.sameIdentityAs(Meta(Some("y"), posA, None, Nil)))
    assert(!a.sameIdentityAs(Meta(None, posA, None, Nil)))
  }

  test("declaration sameness is anchored on position and doc") {
    val a = Meta(Some("x"), posA, None, Nil)
    assert(a.sameDclAs(Meta(Some("x"), posA, None, Nil)))
    assert(!a.sameDclAs(Meta(Some("x"), posB, None, Nil)))
    assert(!a.sameDclAs(Meta(Some("x"), posA, Some("a doc comment"), Nil)))
  }
end MetaSpec
