package DFiant.lib.stream

import DFiant._

abstract class RVStream(streamDir: StreamDir, nameFlatten: DFOwner.NameFlatten = DFOwner.NameFlatten.UnderscoreSuffix)(
  implicit ctx : ContextOf[RVStream]
) extends DFInterface(nameFlatten) {
  private def SourceDir(portDir : PortDir) : DclDir = streamDir match {
    case SOURCE => portDir
    case SINK => portDir match {
      case IN => OUT
      case OUT => IN
    }
    case FLOW => VAR
  }
//  final val bubbleStall = DFBit() <> MasterDir(OUT)  init false
  final val valid       = DFBit() <> SourceDir(OUT) //init false
  final val ready       = DFBit() <> SourceDir(IN)  //init false
  DefaultDclDir <> SourceDir(OUT)
}

object Stream {
//  def apply[T <: DFAny.Type](payloadType : T)(implicit ctx : ContextOf[DFStream[_]]) : DFStream[T] = new DFStream[T](payloadType)
//  @df class Checker[T <: DFAny.Type](_stream : DFStream[T]) extends DFSimulator {
//    private val stream = new DFStream(_stream.payload.dfType) <> IN
//    private object StreamState extends EnumType.Auto {
//      val IDLE, READY, VALID, FIRE = Entry()
//    }
//    import StreamState._
//    import stream._
//    private val state = DFEnum(StreamState) init IDLE
//    matchdf(state)
//      .casedf(IDLE){
//        ifdf(valid && ready){state := FIRE}
//        ifdf(valid && !ready){state := VALID}
//        ifdf(!valid && ready){state := READY}
//      }
//      .casedf(READY){
//        ifdf(!valid && !ready){
//          state := IDLE
//          sim.report(msg"Unexpected lowering of READY", sim.Warning)
//        }
//        ifdf(valid && !ready){state := VALID}
//        ifdf(!valid && ready){state := READY}
//      }
//      .casedf(VALID){
//
//      }
//      .casedf(FIRE){
//
//      }
//    atOwnerDo(_stream <> stream)
//  }

}

//object IOExample {
//  @df class Regular extends DFDesign {
//    final val i = DFUInt(8) <> IN
//    final val o = DFUInt(8) <> OUT
//    o <> i
//  }
//
//  @df class Stream extends DFDesign {
//    final val i = DFUInt(8) <> SOURCE
//    final val o = DFUInt(8) <> SINK
//    o <> i
//  }
//}
//
//object IOPrevInitExample {
//  @df class Regular extends DFDesign {
//    final val i = DFUInt(8) <> IN init 7
//    final val o = DFUInt(8) <> OUT
//    o <> i.prev
//  }
//
//  @df class Stream extends DFDesign {
//    final val i = new DFStream(DFUInt.Type(8)) <> ASIS
//    final val o = new DFStream(DFUInt.Type(8)) <> FLIP
//    private val i_prev_payload = DFUInt(8) init 7 //init according to the init of i
//    private val i_prev_bubbleStall = DFBit() init 0 //init 0 (no stall) iff i is initialized
//    private val i_prev_valid = DFBit() init 1 //a prev will always be initialized valid
//    private val i_prev_ready = DFBit() init 0//a prev will always be initialized ready
//
//    o.valid := i_prev_valid.prev
//    o.payload := i_prev_payload.prev
//    o.bubbleStall := i_prev_bubbleStall.prev
//    i.ready := i_prev_ready.prev
//
//    ifdf(i_prev_valid.prev && i_prev_ready.prev) { //firing of state change
//      i_prev_payload := i.payload
//      i_prev_bubbleStall := i.bubbleStall
//      i_prev_valid := i.valid
//    }.elseifdf(i.valid) {
//      i_prev_valid := 1
//    }
//    i_prev_ready := o.ready
//  }
//}
//
//object CommulativeSumExample {
//  @df class Regular extends DFDesign {
//    final val i = DFUInt(8) <> IN
//    final val o = DFUInt(8) <> OUT
//    final val sum = DFUInt(8) init 0
//    sum := sum.prev + i
//    o <> sum
//  }
//
//  @df class Stream extends DFDesign {
//    final val i = new DFStream(DFUInt.Type(8)) <> ASIS
//    final val o = new DFStream(DFUInt.Type(8)) <> FLIP
//    private val sum_payload = DFUInt(8) init 0
//    private val sum_bubbleStall = DFBit() init 0
//    private val sum_valid = DFBit() init 1
//    private val sum_ready = DFBit() init 0
//
//    sum_payload := sum_payload.prev + i.payload
//    sum_valid := sum_valid.prev && i.valid
//    sum_bubbleStall := sum_bubbleStall.prev || i.bubbleStall
//    i.ready := sum_ready.prev
//
//    o.payload := sum_payload
//    o.valid := sum_valid
//    o.bubbleStall := sum_bubbleStall
//    sum_ready := o.ready
//
//    //State handling
//    /////////////////
//    ifdf (!sum_valid || !sum_ready) {
//      sum_bubbleStall := 1
//      sum_payload := sum_payload.prev
//    }
//    ifdf (sum_bubbleStall) {
//    }
//    sum_valid := 1
//
//  }
//}
//
//
//object GenConstExample {
//  @df class Regular extends DFDesign {
//    final val i = DFUInt(8) <> IN
//    final val o = DFUInt(8) <> OUT
//    i.consume() //implicit
//    o := 5
//  }
//
//  @df class Stream extends DFDesign {
//    final val i = new DFStream(DFUInt.Type(8)) <> ASIS
//    final val o = new DFStream(DFUInt.Type(8)) <> FLIP
//    i.ready := 1 //i.consume
//    o.payload := 5
//    o.valid := 1
//    o.bubbleStall := 0
//  }
//}
//
//
//object JoinAddExample {
//  @df class Regular extends DFDesign {
//    final val iL = DFUInt(8) <> IN
//    final val iR = DFUInt(8) <> IN
//    final val o = DFUInt(8) <> OUT
//    o := iL + iR //has implicit join
//  }
//
//  @df class Stream extends DFDesign {
//    final val iL = new DFStream(DFUInt.Type(8)) <> ASIS
//    final val iR = new DFStream(DFUInt.Type(8)) <> ASIS
//    final val o = new DFStream(DFUInt.Type(8)) <> FLIP
//    iL.ready := o.ready
//    iR.ready := o.ready
//    o.valid := iL.valid && iR.valid
//    o.payload := iL.payload + iR.payload
//    o.bubbleStall := iL.bubbleStall || iR.bubbleStall
//  }
//}

