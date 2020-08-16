/*
 *     This file is part of DFiant.
 *
 *     DFiant is free software: you can redistribute it and/or modify
 *     it under the terms of the Lesser GNU General Public License as published by
 *     the Free Software Foundation, either version 3 of the License, or
 *     any later version.
 *
 *     DFiant is distributed in the hope that it will be useful,
 *     but WITHOUT ANY WARRANTY; without even the implied warranty of
 *     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *     Lesser GNU General Public License for more details.
 *
 *     You should have received a copy of the Lesser GNU General Public License
 *     along with DFiant.  If not, see <https://www.gnu.org/licenses/>.
 */

package DFiant
package compiler.csprinter
import compiler.printer.formatter._

final case class CompactCodeString(
  seq : Seq[CompactCodeString.Part]
) extends DFMember.CustomTagOf[DFMember] {
  def codeString(implicit printer : CSPrinter, owner : DFOwner) : String = seq.map {
    case CompactCodeString.MemberPart(ref) => ref.get match {
      case v : DFAny => v.refCodeString.applyBrackets()
      case o : DFDesign.Block => o.designType
      case m : DFMember => m.typeName
    }
    case CompactCodeString.CSPrintPart(func) => func(printer.config)
    case CompactCodeString.StringPart(string) => string
  }.mkString
}

object CompactCodeString {
  type Ref = DFMember.Ref.Of[Ref.Type, DFMember]
  object Ref {
    trait Type extends DFMember.Ref.Type
    implicit val ev : Type = new Type {}
  }
  sealed trait Part extends Product with Serializable
  final case class StringPart(value : String) extends Part
  final case class MemberPart(ref : Ref) extends Part
  final case class CSPrintPart(func : CSPrinter.Config => String) extends Part
}