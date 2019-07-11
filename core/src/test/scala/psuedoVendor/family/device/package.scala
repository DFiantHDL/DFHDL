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

//package psuedoVendor.family
//
//import DFiant._
//import DFComponent.Implementation
//package object device {
//  implicit object targetLib extends DFiant.basiclib.DFBasicLib {
//    implicit def `evU+U`(implicit ctx : Implementation.Context) : Implementation[`U+U`] = ifc => {
//      import ifc._
//    }
//    implicit def `evU-U`(implicit ctx : Implementation.Context) : Implementation[`U-U`] = ifc => {
//      import ifc._
//    }
//    implicit def `evU*U`(implicit ctx : Implementation.Context) : Implementation[`U*U`] = ifc => {
//      import ifc._
//    }
//
//    implicit def `evU==U`(implicit ctx : Implementation.Context) : Implementation[`U==U`] = ifc => {
//      import ifc._
//    }
//    implicit def `evU!=U`(implicit ctx : Implementation.Context) : Implementation[`U!=U`] = ifc => {
//      import ifc._
//    }
//    implicit def `evU<U`(implicit ctx : Implementation.Context) : Implementation[`U<U`] = ifc => {
//      import ifc._
//    }
//    implicit def `evU>U`(implicit ctx : Implementation.Context) : Implementation[`U>U`] = ifc => {
//      import ifc._
//    }
//    implicit def `evU<=U`(implicit ctx : Implementation.Context) : Implementation[`U<=U`] = ifc => {
//      import ifc._
//    }
//    implicit def `evU>=U`(implicit ctx : Implementation.Context) : Implementation[`U>=U`] = ifc => {
//      import ifc._
//    }
//
//    implicit def `evE==E`[E <: Enum](implicit ctx : Implementation.Context) : Implementation[`E==E`[E]] = ifc => {
//      import ifc._
//    }
//    implicit def `evE!=E`[E <: Enum](implicit ctx : Implementation.Context) : Implementation[`E!=E`[E]] = ifc => {
//      import ifc._
//    }
//
//  }
//}
