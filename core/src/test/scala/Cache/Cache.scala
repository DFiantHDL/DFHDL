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

//package Cache
//
//import DFiant._
//
//trait Cache extends DFDesign {
//  final protected val req     = DFStruct(MemReq())  <> IN
//  final protected val resp    = Data()              <> OUT
//  final protected val memReq  = DFStruct(MemReq())  <> OUT
//  final protected val memResp = Data()              <> IN
//
//  final protected val vArray = List.fill(Rows)(DFBool() init false)
//  final protected val tagArray = List.fill(Rows)(Tag())
//  final protected val dataArray = List.fill(Rows)(Data())
//  final protected val status = DFEnum(CacheStatus) init CacheStatus.Rdy
//}
//
//
//trait BlockingICache extends Cache {
//  private val addr = req.fields.addr
//  private val idx = addr.lsbits(addr.width-2).uint
//  private val tag = addr.msbits(addr.width-1)
//  private val dataOut = Data()
//  private val valid = DFBool()
//  private val tagMatch = DFBool()
//  resp.dontProduce()
//
//  matchdf(status)
//    .casedf(CacheStatus.Rdy) {
//      vArray.foreachdf(idx){case v => valid := v}
//      tagArray.foreachdf(idx){case t => tagMatch := t == tag}
//      dataArray.foreachdf(idx){case d => dataOut := d}
//      ifdf (valid && tagMatch) {
//        resp := dataOut
//      }.elsedf {
//        memReq.fields.addr := req
//        memReq.fields.op := MemOp.Ld
//        status := CacheStatus.Fill
//      }
//    }
//    .casedf(CacheStatus.Fill) {
//      vArray.foreachdf(idx){case v => v := true}
//      tagArray.foreachdf(idx){case t => t := tag}
//      dataArray.foreachdf(idx){case d => d := memResp}
//      status := CacheStatus.Rdy
//    }
//}
//
//trait BlockingDCache extends Cache {
//  private val addr = req.fields.addr
//  private val op = req.fields.op
//  private val data = req.fields.data
//  private val idx = addr.lsbits(addr.width-2).uint
//  private val tag = addr.msbits(addr.width-1)
//  private val dataOut = Data()
//  private val valid = DFBool()
//  private val tagMatch = DFBool()
//  resp.dontProduce()
//
//  matchdf(status)
//    .casedf(CacheStatus.Rdy) {
//      vArray.foreachdf(idx){case v => valid := v}
//      tagArray.foreachdf(idx){case t => tagMatch := t == tag}
//      dataArray.foreachdf(idx){case d => dataOut := d}
//      ifdf (valid && tagMatch && op == MemOp.Ld) {
//        resp := dataOut
//      }.elseifdf(valid && op == MemOp.St) {
//        memReq.fields.addr := tagMatch ## idx.bits ## b"00"
//        memReq.fields.op := MemOp.St
//        memReq.fields.data := dataOut
//        status := CacheStatus.WrBack
//      }.elsedf {
//        memReq.fields.addr := req
//        memReq.fields.op := MemOp.Ld
//        status := CacheStatus.Fill
//      }
//    }
//    .casedf(CacheStatus.WrBack) {
//      memReq.fields.addr := req
//      memReq.fields.op := MemOp.Ld
//      status := CacheStatus.Fill
//    }
//    .casedf(CacheStatus.Fill) {
//      vArray.foreachdf(idx){case v => v := true}
//      tagArray.foreachdf(idx){case t => t := tag}
//      dataArray.foreachdf(idx){case d => d := memResp}
//      status := CacheStatus.Rdy
//    }
//}
//case class MemReq()(implicit ctx : DFFields.Context) extends DFFields {
//  final val addr = Addr()
//  final val data = Data()
//  final val op = DFEnum(MemOp)
//}
//
//
