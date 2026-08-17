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
