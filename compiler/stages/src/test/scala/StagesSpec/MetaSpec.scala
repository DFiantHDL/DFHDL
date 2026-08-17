package StagesSpec

import munit.FunSuite
import dfhdl.*
import dfhdl.compiler.ir
import dfhdl.compiler.ir.Meta
import dfhdl.internals.{NoTopAnnotIsRequired, Position}

/** the struct doc */
case class TMCStruct(x: UInt[8] <> VAL, y: Bit <> VAL) extends Struct
val TMCGlobal: UInt[8] <> CONST = 7
enum TMCEnum extends Encoded:
  case Alpha, Beta, Gamma
case class TMCOpaque() extends Opaque(UInt(8))

/** Pins `Meta`'s two comparison notions (`Meta` has no `CanEqual`, so call sites must name one):
  *   - `sameIdentityAs` excludes position and doc, which can drift under a valid elaboration-cache
  *     entry (the code digest hashes typed trees and never sees formatting or comments), while the
  *     name, namespace, and annotations participate. `equals`/`hashCode` implement it, so member
  *     value-equality (cache adoption, global unification) composes through it.
  *   - `sameDclAs` compares all fields; "same declaration" is anchored on position.
  *
  * Also pins the declaration meta captured for named DFTypes: the derivation macros record the
  * name, the enclosing Scala package (namespace), the declaration position, and the doc comment for
  * structs and enums; the opaque path is runtime-instance based, so it records name and namespace
  * only. Tuples are structural: name only, root namespace.
  */
class MetaSpec extends FunSuite, NoTopAnnotIsRequired:
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

  test("identity includes the namespace") {
    val a = Meta(Some("x"), posA, None, Nil, "pkg.a")
    assert(!a.sameIdentityAs(Meta(Some("x"), posA, None, Nil, "pkg.b")))
    assert(!a.sameDclAs(Meta(Some("x"), posA, None, Nil, "pkg.b")))
    assert(a.sameIdentityAs(Meta(Some("x"), posB, Some("doc"), Nil, "pkg.a")))
  }

  test("declaration sameness is anchored on position and doc") {
    val a = Meta(Some("x"), posA, None, Nil)
    assert(a.sameDclAs(Meta(Some("x"), posA, None, Nil)))
    assert(!a.sameDclAs(Meta(Some("x"), posB, None, Nil)))
    assert(!a.sameDclAs(Meta(Some("x"), posA, Some("a doc comment"), Nil)))
  }

  class Top extends DFDesign:
    val s = TMCStruct <> VAR
    val e = TMCEnum <> VAR
    val o = TMCOpaque <> VAR
    val t = (UInt(8), Bit) <> VAR
    val loc = UInt(8) <> VAR init TMCGlobal

  lazy val dclTypes: Map[String, ir.DFType] =
    val db = (new Top).getDB
    db.subDBs.values.toList.flatMap { sub =>
      sub.members.collect { case dcl: ir.DFVal.Dcl =>
        dcl.getName(using sub.getSet) -> dcl.dfType
      }
    }.toMap

  test("struct meta: name, namespace, position, doc") {
    val meta = dclTypes("s").asInstanceOf[ir.DFStruct].meta
    assertEquals(meta.name, "TMCStruct")
    assertEquals(meta.namespace, "StagesSpec")
    assert(meta.position.file.endsWith("MetaSpec.scala"), meta.position.toString)
    assert(meta.docOpt.nonEmpty && meta.comment.contains("the struct doc"), meta.docOpt.toString)
  }

  test("enum meta: name, namespace, position") {
    val meta = dclTypes("e").asInstanceOf[ir.DFEnum].meta
    assertEquals(meta.name, "TMCEnum")
    assertEquals(meta.namespace, "StagesSpec")
    assert(meta.position.file.endsWith("MetaSpec.scala"), meta.position.toString)
  }

  test("opaque meta: name, namespace, position") {
    val opaque = dclTypes("o").asInstanceOf[ir.DFOpaque]
    val meta = opaque.meta
    assertEquals(meta.name, "TMCOpaque")
    assertEquals(meta.namespace, "StagesSpec")
    assert(meta.position.file.endsWith("MetaSpec.scala"), meta.position.toString)
    // a GENERAL opaque is identified by its meta (name + namespace) like structs and
    // enums; only magnets carry a per-instance id
    assertEquals(opaque.id, 0)
  }

  test("tuple struct meta: structural, root namespace") {
    val meta = dclTypes("t").asInstanceOf[ir.DFStruct].meta
    assertEquals(meta.name, "DFTuple2")
    assertEquals(meta.namespace, "")
  }

  test("a global constant carries its package; a design-scoped value does not") {
    val db = (new Top).getDB
    val globalMeta = db.subDBs.values.toList.flatMap(_.membersGlobals).collectFirst {
      case g: ir.DFVal if g.meta.name == "TMCGlobal" => g.meta
    }.get
    assertEquals(globalMeta.namespace, "StagesSpec")
    val locMeta = db.subDBs.values.toList.flatMap(_.members).collectFirst {
      case dcl: ir.DFVal.Dcl if dcl.meta.name == "loc" => dcl.meta
    }.get
    assertEquals(locMeta.namespace, "")
  }

  test("namespace placement rules") {
    import dfhdl.compiler.printing.Namespacing.*
    // equal or ancestor (root included) -> global defs file
    assert(isGlobalPlaced("", "veer.core"))
    assert(isGlobalPlaced("veer.core", "veer.core"))
    assert(isGlobalPlaced("veer", "veer.core"))
    assert(!isGlobalPlaced("veer.types", "veer.core"))
    assert(!isGlobalPlaced("veercore", "veer.core")) // prefix of a SEGMENT is not an ancestor
    // package name: namespace relative to the top, joined with `_`
    assertEquals(packageNameOf("veer.veer_types", "veer"), "veer_types")
    assertEquals(packageNameOf("veer.types", "veer.core"), "types")
    assertEquals(packageNameOf("dfhdl.lib.crypto.aes", "myproj"), "dfhdl_lib_crypto_aes")
    assertEquals(packageNameOf("b.util", "a"), "b_util")
    // the documented residual clash: `top.x` and root-level `x` both map to "x"
    assertEquals(packageNameOf("a.x", "a"), packageNameOf("x", "a"))
  }
end MetaSpec
