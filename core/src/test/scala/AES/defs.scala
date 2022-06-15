package AES
import dfhdl.*

//////////////////////////////////////////////////////////////////////////////////////////////////////
// AES Byte
//////////////////////////////////////////////////////////////////////////////////////////////////////
object AESByte extends Opaque(Byte)
type AESByte = AESByte.type

//Non-linear substitution table used in several byte substitution transformations and in the Key Expansion
//routine to perform a one-for-one substitution of a byte value.
val sboxLookupTable: Vector[AESByte <> TOKEN] = Vector(
  "63", "7c", "77", "7b", "f2", "6b", "6f", "c5", "30", "01", "67", "2b", "fe", "d7", "ab", "76",
  "ca", "82", "c9", "7d", "fa", "59", "47", "f0", "ad", "d4", "a2", "af", "9c", "a4", "72", "c0",
  "b7", "fd", "93", "26", "36", "3f", "f7", "cc", "34", "a5", "e5", "f1", "71", "d8", "31", "15",
  "04", "c7", "23", "c3", "18", "96", "05", "9a", "07", "12", "80", "e2", "eb", "27", "b2", "75",
  "09", "83", "2c", "1a", "1b", "6e", "5a", "a0", "52", "3b", "d6", "b3", "29", "e3", "2f", "84",
  "53", "d1", "00", "ed", "20", "fc", "b1", "5b", "6a", "cb", "be", "39", "4a", "4c", "58", "cf",
  "d0", "ef", "aa", "fb", "43", "4d", "33", "85", "45", "f9", "02", "7f", "50", "3c", "9f", "a8",
  "51", "a3", "40", "8f", "92", "9d", "38", "f5", "bc", "b6", "da", "21", "10", "ff", "f3", "d2",
  "cd", "0c", "13", "ec", "5f", "97", "44", "17", "c4", "a7", "7e", "3d", "64", "5d", "19", "73",
  "60", "81", "4f", "dc", "22", "2a", "90", "88", "46", "ee", "b8", "14", "de", "5e", "0b", "db",
  "e0", "32", "3a", "0a", "49", "06", "24", "5c", "c2", "d3", "ac", "62", "91", "95", "e4", "79",
  "e7", "c8", "37", "6d", "8d", "d5", "4e", "a9", "6c", "56", "f4", "ea", "65", "7a", "ae", "08",
  "ba", "78", "25", "2e", "1c", "a6", "b4", "c6", "e8", "dd", "74", "1f", "4b", "bd", "8b", "8a",
  "70", "3e", "b5", "66", "48", "03", "f6", "0e", "61", "35", "57", "b9", "86", "c1", "1d", "9e",
  "e1", "f8", "98", "11", "69", "d9", "8e", "94", "9b", "1e", "87", "e9", "ce", "55", "28", "df",
  "8c", "a1", "89", "0d", "bf", "e6", "42", "68", "41", "99", "2d", "0f", "b0", "54", "bb", "16"
).map(byte => h"$byte".as(AESByte))

extension (lhs: AESByte <> VAL)
  // The addition of two elements in a finite field is achieved by “adding” the coefficients for the
  // corresponding powers in the polynomials for the two elements. The addition is performed with
  // the XOR operation.
  def +(rhs: AESByte <> VAL)(using DFC): AESByte <> VAL =
    (lhs.actual ^ rhs.actual).as(AESByte)

  private def xtime()(using DFC): AESByte <> VAL =
    val shifted = lhs.actual << 1
    if (lhs.actual(7)) (shifted ^ h"1b").as(AESByte)
    else shifted.as(AESByte)

  // In the polynomial representation, multiplication in GF(2^8) corresponds with the multiplication of
  // polynomials modulo an irreducible polynomial of degree 8. A polynomial is irreducible if its only
  // divisors are one and itself. For the AES algorithm, this irreducible polynomial is
  // m(x) = x^8 + x^4 + x^3 + x + 1, or {01}{1b} in hexadecimal notation.
  def *(that: AESByte <> TOKEN)(using DFC): AESByte <> VAL =
    val p = AESByte <> VAR
    p := all(0).as(AESByte)
    val (ret, _) = (0 until 8).foldLeft[(AESByte <> VAL, AESByte <> VAL)]((p, lhs)) {
      case ((p, a), i) if that.bits(i) => (p + a, a.xtime())
      case ((p, a), _)                 => (p, a.xtime())
    }
    ret

  // Non-linear substitution table used in several byte substitution transformations and in the Key Expansion
  // routine to perform a one-for-one substitution of a byte value.
  def sbox(using DFC): AESByte <> VAL =
    val lookup = (AESByte X sboxLookupTable.length) <> VAR init sboxLookupTable
    lookup(lhs.actual)

end extension
