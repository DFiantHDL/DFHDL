///*
// *     This file is part of DFiant.
// *
// *     DFiant is free software: you can redistribute it and/or modify
// *     it under the terms of the GNU Lesser General Public License as published by
// *     the Free Software Foundation, either version 3 of the License, or
// *     any later version.
// *
// *     DFiant is distributed in the hope that it will be useful,
// *     but WITHOUT ANY WARRANTY; without even the implied warranty of
// *     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// *     GNU Lesser General Public License for more details.
// *
// *     You should have received a copy of the GNU Lesser General Public License
// *     along with DFiant.  If not, see <https://www.gnu.org/licenses/>.
// */
//
//package DFiant.internals
//
//trait DFAnyMember {
//  protected[DFiant] trait __DevDFAnyMember {
////    final protected[DFiant] def isNotDiscovered : Boolean = !discovered
//    val discovered : CacheBoxRW[Boolean]
//    final private[internals] def justAHack = discoveryDependencies
//    protected def preDiscoveryRun() : Unit = {}
//    protected def postDiscoveryRun() : Unit = {}
//    final protected def discover() : Unit = {
//      if (!discovered) {
//        discovered.set(true)
//        val dependencies = discoveryDependencies
//        preDiscoveryRun()
//        dependencies.foreach(d => d.__dev.discover())
//        postDiscoveryRun()
//      }
//    }
//    private def discoverDependencies() : Unit = discoveryDependencies.foreach(d => d.__dev.discover())
//    final private[DFiant] def rediscoverDependencies() : Unit = if (discovered) discoverDependencies()
//
//  }
//  private[DFiant] lazy val __dev : __DevDFAnyMember = ???
//}
