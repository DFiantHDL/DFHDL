package DFiant.lib

import DFiant._

class Stream(payloadType : DFAny.Type)(implicit ctx : ContextOf[Stream]) extends DFInterface {
//  final val payload = DFAny.Port.In(payloadType)
  final val valid = DFBit() <> IN init false
//  final val ready = DFBit() <> OUT init false
}
