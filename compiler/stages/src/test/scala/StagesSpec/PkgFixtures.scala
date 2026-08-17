package StagesSpec

import dfhdl.*

// General-global declarations (namespace `StagesSpec`, the specs' top-design namespace),
// referenced from `typespkg1` to pin the package -> general-globals dependency direction.
// Uniquely named to avoid collisions with anything else under StagesSpec.
case class GlbNsStruct(g: Bits[2] <> VAL) extends Struct
val GlbNsConst: UInt[8] <> CONST = 3

// Sibling packages in one file: both land in dedicated packages (`typespkg1`,
// `typespkg2`) under the placement rules, with typespkg2 referencing typespkg1, and
// typespkg1 referencing the general globals above.
package typespkg1 {
  case class PkgStruct(a: Bits[8] <> VAL, b: Bit <> VAL, g: GlbNsStruct <> VAL) extends Struct
  enum PkgEnum extends Encoded:
    case P0, P1, P2
  case class PkgOpaque() extends Opaque(Bits(4))
  val PkgConst: UInt[8] <> CONST = GlbNsConst + 39
  def pkgCalc(arg: UInt[8] <> CONST): UInt[8] <> CONSTRET = arg + 1
  val PkgDerived: UInt[8] <> CONST = pkgCalc(PkgConst)
}

package typespkg2 {
  case class PkgWrap(s: typespkg1.PkgStruct <> VAL, n: UInt[8] <> VAL) extends Struct
  val PkgWide: UInt[8] <> CONST = typespkg1.pkgCalc(typespkg1.PkgDerived)
}

// Cross-package homographs: two sibling packages declaring the SAME simple names (and, for
// `Shared`, structurally different types under that name). Every packaged reference is emitted
// qualified, so name uniqueness is scoped per package and neither side is renamed.
// NOTE: the static functions are deliberately named apart. A method is a design block, and
// same-named design blocks are enumerated (`f_0`, `f_1`) by elaboration, which is not
// package-aware — so a packaged method name is still globally unique, unlike a type or a
// constant name.
package dualpkg1 {
  case class Shared(v: Bits[4] <> VAL) extends Struct
  val SharedConst: UInt[8] <> CONST = 1
  def calc1(arg: UInt[8] <> CONST): UInt[8] <> CONSTRET = arg + 10
  val SharedDerived: UInt[8] <> CONST = calc1(SharedConst)
}

package dualpkg2 {
  case class Shared(v: Bits[8] <> VAL) extends Struct
  val SharedConst: UInt[8] <> CONST = 2
  def calc2(arg: UInt[8] <> CONST): UInt[8] <> CONSTRET = arg + 20
  val SharedDerived: UInt[8] <> CONST = calc2(SharedConst)
}
