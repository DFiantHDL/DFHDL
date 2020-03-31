///*
// *     This file is part of DFiant.
// *
// *     DFiant is free software: you can redistribute it and/or modify
// *     it under the terms of the Lesser GNU General Public License as published by
// *     the Free Software Foundation, either version 3 of the License, or
// *     any later version.
// *
// *     DFiant is distributed in the hope that it will be useful,
// *     but WITHOUT ANY WARRANTY; without even the implied warranty of
// *     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// *     Lesser GNU General Public License for more details.
// *
// *     You should have received a copy of the Lesser GNU General Public License
// *     along with DFiant.  If not, see <https://www.gnu.org/licenses/>.
// */
//
//package fpga2020
//import DFiant._
//
//trait Box extends DFDesign {
//  val iT = DFSInt[16] <> IN
//  val iB = DFSInt[16] <> IN
//  val oT = DFSInt[16] <> OUT
//  val oB = DFSInt[16] <> OUT
//}
//trait BoxLeft extends Box {
//  iT.prev <> oT
//  oB := iB
//}
//trait BoxRight extends Box {
//  iT <> oT
//  oB := iB.prev
//}
//trait BoxParent extends Box {
//  iT init (5, 7)
//  iB init (2, 6)
//  val boxL = new BoxLeft {}
//  val boxR = new BoxRight {}
//  boxL.iT <> iT
//  boxL.iB <> iB
//  boxR.oT <> oT
//  boxR.oB <> oB
//}
//trait BoxParDirect extends BoxParent {
//  boxL.oT <> boxR.iT
//  boxL.oB <> boxR.iB
//}
//trait BoxParCross extends BoxParent {
//  boxL.oT <> boxR.iB
//  boxL.oB <> boxR.iT
//}
//
//object BoxTopApp extends DFApp {
//  val boxDirect = new BoxParDirect {}.printVHDLString
//  val boxCross = new BoxParCross {}.printVHDLString
//}
