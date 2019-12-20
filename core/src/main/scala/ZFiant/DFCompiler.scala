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

package ZFiant
import DFiant.internals._

object DFCompiler {
  implicit class Discovery(designDB : DFDesign.DB) {
    def discovery : DFDesign.DB = {
      ???
    }
  }

  implicit class Naming(designDB : DFDesign.DB) {
    def fixNames : DFDesign.DB = {
      import designDB.getter
      val patchList = designDB.members.collect {
        case m : DFAny if (m.meta.name == m.getOwner.meta.name) && (m.meta.namePosition == m.getOwner.meta.namePosition) =>
          m -> m.annonimize
      }
      designDB.patch(patchList.toMap)
    }
  }

  implicit class CodeString(designDB : DFDesign.DB) {
//    val designStrings : Map[String, Map]
    designDB.members.collect {
      case m : DFDesign =>
    }
  }
}