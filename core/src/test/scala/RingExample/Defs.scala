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

//package RingExample
//
//import DFiant._
//
//object Defs {
//  final val payloadSizeBits = 128
//  final val flitSizeBits = 16
//  final val flitsNum = payloadSizeBits / flitSizeBits
//  protected[RingExample] var nodesNum = 8
//}
//import Defs._
//
//object Response extends Enumeration {
//  type Response = Value
//  val NACK, ACK = Value
//}
//import Response._
//
///////////////////////////////////////////////////////////////////////////////////////
//trait DFFlitCtrl extends DFStruct[DFFlitCtrl] {
//  type TField1 = DFEnum[Response]
//  val resp = insert(DFEnum(Response))
//  val retry = insert(DFBits(flitSizeBits-resp.width))
//}
//
//object DFFlitCtrl {
//  def apply() = DFStruct[DFFlitCtrl]()
//}
///////////////////////////////////////////////////////////////////////////////////////
//
//
///////////////////////////////////////////////////////////////////////////////////////
//trait DFFlitPayload extends DFTaggedUnion[DFFlitPayload] {
//  type TField1 = DFFlitCtrl
//
//  val data = insert(DFBits(flitSizeBits))
//  val ctrl = insert(DFFlitCtrl())
//}
//
//object DFFlitPayload {
//  def apply() = DFTaggedUnion[DFFlitPayload]()
//}
///////////////////////////////////////////////////////////////////////////////////////
//
//
///////////////////////////////////////////////////////////////////////////////////////
//trait DFPacket extends DFStruct[DFPacket] {
//  val nodesNum = Defs.nodesNum
//  val peer = insert(DFUInt.rangeUntil(nodesNum))
//  val msg = insert(DFBits(payloadSizeBits))
//
//  def toFlits(nodeID : Int) : DFFlit = {
//    val flit = DFFlit(nodesNum)
//    for (i <- 0 until flitsNum) {
//      flit.ctrl.assignNext(i, 0)
//      flit.src.assignNext(i, nodeID)
//      flit.dst.assignNext(i, peer)
//      flit.idx.assignNext(i, i)
//      flit.payload.data.assignNext(i, msg((i+1)*flitSizeBits-1, i*flitSizeBits))
//    }
//    flit
//  }
//}
//
//object DFPacket {
//  def apply(nodesNum : Int) = DFStruct[DFPacket]()
//}
///////////////////////////////////////////////////////////////////////////////////////
//
//
///////////////////////////////////////////////////////////////////////////////////////
//trait DFFlit extends DFStruct[DFFlit] {
//  type TField1 = DFFlitPayload
//  val nodesNum : Int = Defs.nodesNum
//  val ctrl = insert(DFBool())
//  val src = insert(DFUInt.rangeUntil(nodesNum))
//  val dst = insert(DFUInt.rangeUntil(nodesNum))
//  val payload = insert(DFFlitPayload())
//  val idx = insert(DFUInt.rangeUntil(Defs.flitsNum))
//
//  def toPacket(nodeID : Int) : DFPacket =  {
//    ??? //TODO: fix implementation (next improperly used)
//    val packet = DFPacket(nodesNum)
//    for (i <- 0 until flitsNum) {
//      ctrl.next(i).consume()
//      src.next(i).consume()
//      dst.next(i).consume()
//      idx.next(i).consume()
//      if (i == 0)
//        packet.peer := src
//      packet.msg((i+1)*flitSizeBits-1, i*flitSizeBits) := payload.data.next(i)
//    }
//    packet
//  }
//}
//
//object DFFlit {
//  def apply(nodesNum : Int) = DFStruct[DFFlit]()
//}
///////////////////////////////////////////////////////////////////////////////////////
//
//
