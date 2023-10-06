package AES

import dfhdl.{apply => _, *}
export dfhdl.apply

import scala.annotation.targetName
import scala.collection.mutable.ArrayBuffer
//////////////////////////////////////////////////////////////////////////////////////////////////////
// AES Byte
//////////////////////////////////////////////////////////////////////////////////////////////////////
case class AESByte() extends Opaque(Byte)

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
  @targetName("addByte")
  @inline def +(rhs: AESByte <> VAL): AESByte <> VAL =
    (lhs.actual ^ rhs.actual).as(AESByte)

  private def xtime: AESByte <> VAL =
    val shifted = lhs.actual << 1
    if (lhs.actual(7)) (shifted ^ h"1b").as(AESByte)
    else shifted.as(AESByte)

  // Non-linear substitution table used in several byte substitution transformations and in the Key Expansion
  // routine to perform a one-for-one substitution of a byte value.
  def sbox: AESByte <> VAL =
    val lookup = AESByte X sboxLookupTable.length const sboxLookupTable
    lookup(lhs.actual)
end extension

extension (lhs: Byte <> TOKEN)
  // In the polynomial representation, multiplication in GF(2^8) corresponds with the multiplication of
  // polynomials modulo an irreducible polynomial of degree 8. A polynomial is irreducible if its only
  // divisors are one and itself. For the AES algorithm, this irreducible polynomial is
  // m(x) = x^8 + x^4 + x^3 + x + 1, or {01}{1b} in hexadecimal notation.
  @targetName("mulByte")
  def *(rhs: AESByte <> VAL): AESByte <> VAL =
    val a = LazyList.iterate(rhs)(_.xtime)
    (0 until 8).foldLeft[AESByte <> VAL](all(0).as(AESByte)):
      case (p, i) if lhs.bits(i) => p + a(i)
      case (p, _)                => p

//////////////////////////////////////////////////////////////////////////////////////////////////////
// AES Word
//////////////////////////////////////////////////////////////////////////////////////////////////////
case class AESWord() extends Opaque(AESByte X 4)

extension (lhs: AESWord <> VAL)
  @targetName("addWord")
  @inline def +(rhs: AESWord <> VAL): AESWord <> VAL =
    (lhs.bits ^ rhs.bits).as(AESWord)

  // Function used in the Key Expansion routine that takes a four-byte input word and applies
  // an S-box to each of the four bytes to produce an output word.
  def subWord: AESWord <> VAL =
    lhs.actual.elements.map(_.sbox).as(AESWord)

  // Function used in the Key Expansion routine that takes a four-byte word and performs a cyclic permutation.
  def rotWord: AESWord <> VAL =
    val elms = lhs.actual.elements
    (elms.drop(1) :+ elms.head).as(AESWord)
end extension

//////////////////////////////////////////////////////////////////////////////////////////////////////
// AES Matrix Data Structure
//////////////////////////////////////////////////////////////////////////////////////////////////////
//TODO: fix if https://github.com/lampepfl/dotty/issues/17036 is resolved
abstract class AESMatrix[C <: Int with Singleton](val colNum: C)
    extends Opaque[AESWord X C](AESWord X colNum)
extension [C <: Int with Singleton](matrix: AESMatrix[C] <> VAL)
  @inline def apply(colIdx: Int): AESWord <> VAL = matrix.actual(colIdx)
  @inline def apply(rowIdx: Int, colIdx: Int): AESByte <> VAL = matrix.actual(colIdx).actual(rowIdx)

//////////////////////////////////////////////////////////////////////////////////////////////////////
// AES Dimensions & Data Structures
//////////////////////////////////////////////////////////////////////////////////////////////////////
//Nb = Number of columns (32-bit words) comprising the State. For this standard, Nb = 4
final val Nb = 4
//Nk = Number of 32-bit words comprising the Cipher Key. For this standard, Nk = 4, 6, or 8.
final val Nk = 4
//Nr = Number of rounds, which is a function of Nk and Nb (which is fixed). For this standard, Nr = 10, 12, or 14.
final val Nr = 10

case class AESState() extends AESMatrix(Nb)
case class AESRoundKey() extends AESMatrix(Nb)
case class AESData() extends AESMatrix(Nb)
case class AESKey() extends AESMatrix(Nk)
case class AESKeySchedule() extends AESMatrix(Nb * (Nr + 1))

//////////////////////////////////////////////////////////////////////////////////////////////////////
// AES State
//////////////////////////////////////////////////////////////////////////////////////////////////////
extension (state: AESState <> VAL)
  // Transformation in the Cipher that processes the State using a non-linear byte substitution
  // table (S-box) that operates on each of the State bytes independently.
  def subBytes: AESState <> VAL =
    Vector
      .tabulate(Nb, 4)((c, r) => state(r, c).sbox)
      .map(_.as(AESWord)).as(AESState)

  // Transformation in the Cipher that processes the State by cyclically shifting the last three rows of
  // the State by different offsets. Note: this algorithm only follows the AES spec, and assumes Nb=4. If
  // this were to change, instead of `c + r` to change it to `c + shift(r, Nb)` that sets the shift
  // according to the general Rijndael algorithm.
  def shiftRows: AESState <> VAL =
    Vector
      .tabulate(Nb, 4)((c, r) => state(r, (c + r) % Nb))
      .map(_.as(AESWord)).as(AESState)

  // Transformation in the Cipher that takes all of the columns of the State and mixes their data
  // (independently of one another) to produce new columns.
  def mixColumns: AESState <> VAL =
    Vector.tabulate(Nb)(c =>
      Vector(
        h"02" * state(0, c) + h"03" * state(1, c) + h"01" * state(2, c) + h"01" * state(3, c),
        h"01" * state(0, c) + h"02" * state(1, c) + h"03" * state(2, c) + h"01" * state(3, c),
        h"01" * state(0, c) + h"01" * state(1, c) + h"02" * state(2, c) + h"03" * state(3, c),
        h"03" * state(0, c) + h"01" * state(1, c) + h"01" * state(2, c) + h"02" * state(3, c)
      ).as(AESWord)
    ).as(AESState)

  // Transformation in the Cipher and Inverse Cipher in which a Round Key is added to the State using an XOR
  // operation. The length of a Round Key equals the size of the State (i.e., for Nb = 4, the Round Key length
  // equals 128 bits/16 bytes).
  @inline def addRoundKey(key: AESRoundKey <> VAL): AESState <> VAL =
    (state.bits ^ key.bits).as(AESState)
end extension

//////////////////////////////////////////////////////////////////////////////////////////////////////
// AES Key
//////////////////////////////////////////////////////////////////////////////////////////////////////
// The round constant word array.
private val Rcon = Vector(
  "00000000", "01000000", "02000000", "04000000", "08000000", "10000000", "20000000", "40000000",
  "80000000", "1B000000", "36000000"
).map(word => h"$word".as(AESWord))

extension (key: AESKey <> VAL)
  def keyExpansion: AESKeySchedule <> VAL =
    val w = ArrayBuffer.tabulate(Nk)(i => key(i))
    for (i <- Nk until Nb * (Nr + 1)) do
      val temp =
        if (i % Nk == 0) w(i - 1).rotWord.subWord + Rcon(i / Nk)
        else if ((Nk > 6) && (i % Nk == 4)) w(i - 1).subWord
        else w(i - 1)
      w += w(i - Nk) + temp
    w.as(AESKeySchedule)

//////////////////////////////////////////////////////////////////////////////////////////////////////
// AES Keyschedule
//////////////////////////////////////////////////////////////////////////////////////////////////////
extension (keySched: AESKeySchedule <> VAL)
  @inline def roundKey(round: Int): AESRoundKey <> VAL =
    Vector.tabulate(Nb)(b => keySched(round * Nb + b)).as(AESRoundKey)

//////////////////////////////////////////////////////////////////////////////////////////////////////
// Cipher
//////////////////////////////////////////////////////////////////////////////////////////////////////
def cipher(data: AESData <> VAL, key: AESKey <> VAL): AESData <> VAL =
  val keySchedule = key.keyExpansion
  val state = (0 to Nr).foldLeft(data.actual.as(AESState))((state, round) =>
    val roundVal =
      if (round == 0) state
      else if (round < Nr) state.subBytes.shiftRows.mixColumns
      else state.subBytes.shiftRows
    roundVal.addRoundKey(keySchedule.roundKey(round))
  )
  state.actual.as(AESData)
