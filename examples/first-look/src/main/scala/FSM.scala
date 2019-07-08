/*
 *     This file is part of DFiant.
 *
 *     DFiant is free software: you can redistribute it and/or modify
 *     it under the terms of the GNU Lesser General Public License as published by
 *     the Free Software Foundation, either version 3 of the License, or
 *     any later version.
 *
 *     DFiant is distributed in the hope that it will be useful,
 *     but WITHOUT ANY WARRANTY; without even the implied warranty of
 *     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *     GNU Lesser General Public License for more details.
 *
 *     You should have received a copy of the GNU Lesser General Public License
 *     along with DFiant.  If not, see <https://www.gnu.org/licenses/>.
 */
package example3

import DFiant._ //Required in any DFiant compilation program

object SeqState extends Enum.Auto { //State entry enumeration
  val S0, S1, S10, S100, S1001 = Entry
}

trait FSM extends DFDesign { //This our `FSM` dataflow design
  val x = DFBool() <> IN  //The input port is a boolean
  val y = DFBool() <> OUT //The output port is a boolean
  val ss = DFEnum(SeqState) init SeqState.S0 //The sequence state

  matchdf(ss) //dataflow match on the state
    .casedf(SeqState.S0) {
      y := 0
      ifdf (x == 0) {
        ss := SeqState.S0
      }.elsedf {
        ss := SeqState.S1
      }
    }.casedf(SeqState.S1) {
      y := 0
      ifdf (x == 0) {
        ss := SeqState.S10
      }.elsedf {
        ss := SeqState.S1
      }
    }.casedf(SeqState.S10) {
      y := 0
      ifdf (x == 0) {
        ss := SeqState.S100
      }.elsedf {
        ss := SeqState.S1
      }
    }.casedf(SeqState.S100) {
      y := 0
      ifdf (x == 0) {
        ss := SeqState.S0
      }.elsedf {
        ss := SeqState.S1001
      }
    }.casedf(SeqState.S1001) {
      y := 1
      ifdf (x == 0) {
        ss := SeqState.S10
      }.elsedf {
        ss := SeqState.S1
      }
    }
}

object FSMApp extends DFApp.VHDLCompiler[FSM] //The FSM compilation program entry-point
