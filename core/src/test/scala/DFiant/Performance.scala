//package DFiant
//
//object Performance {
//  import GlobalDesign._
//
//  val u8 = DFUInt(8)
//  printCompileTime(
//    """
//      |new DFDesign {
//      | val x00 : DFUInt[8] <> IN = TOP
//      | val x01 : DFUInt[8] <> IN = TOP
//      | val x02 : DFUInt[8] <> IN = TOP
//      | val x03 : DFUInt[8] <> IN = TOP
//      | val x04 : DFUInt[8] <> IN = TOP
//      | val x05 : DFUInt[8] <> IN = TOP
//      | val x06 : DFUInt[8] <> IN = TOP
//      | val x07 : DFUInt[8] <> IN = TOP
//      | val x08 : DFUInt[8] <> IN = TOP
//      | val x09 : DFUInt[8] <> IN = TOP
//      | val x10 : DFUInt[8] <> IN = TOP
//      | val x11 : DFUInt[8] <> IN = TOP
//      | val x12 : DFUInt[8] <> IN = TOP
//      | val x13 : DFUInt[8] <> IN = TOP
//      | val x14 : DFUInt[8] <> IN = TOP
//      | val x15 : DFUInt[8] <> IN = TOP
//      | val x16 : DFUInt[8] <> IN = TOP
//      | val x17 : DFUInt[8] <> IN = TOP
//      | val x18 : DFUInt[8] <> IN = TOP
//      | val x19 : DFUInt[8] <> IN = TOP
//      | val x20 : DFUInt[8] <> IN = TOP
//      | val x21 : DFUInt[8] <> IN = TOP
//      | val x22 : DFUInt[8] <> IN = TOP
//      | val x23 : DFUInt[8] <> IN = TOP
//      | val x24 : DFUInt[8] <> IN = TOP
//      | val x25 : DFUInt[8] <> IN = TOP
//      | val x26 : DFUInt[8] <> IN = TOP
//      | val x27 : DFUInt[8] <> IN = TOP
//      | val x28 : DFUInt[8] <> IN = TOP
//      | val x29 : DFUInt[8] <> IN = TOP
//      | val x30 : DFUInt[8] <> IN = TOP
//      | val x31 : DFUInt[8] <> IN = TOP
//      | val x32 : DFUInt[8] <> IN = TOP
//      | val x33 : DFUInt[8] <> IN = TOP
//      | val x34 : DFUInt[8] <> IN = TOP
//      | val x35 : DFUInt[8] <> IN = TOP
//      | val x36 : DFUInt[8] <> IN = TOP
//      | val x37 : DFUInt[8] <> IN = TOP
//      | val x38 : DFUInt[8] <> IN = TOP
//      | val x39 : DFUInt[8] <> IN = TOP
//      | val x40 : DFUInt[8] <> IN = TOP
//      | val x41 : DFUInt[8] <> IN = TOP
//      | val x42 : DFUInt[8] <> IN = TOP
//      | val x43 : DFUInt[8] <> IN = TOP
//      | val x44 : DFUInt[8] <> IN = TOP
//      | val x45 : DFUInt[8] <> IN = TOP
//      | val x46 : DFUInt[8] <> IN = TOP
//      | val x47 : DFUInt[8] <> IN = TOP
//      | val x48 : DFUInt[8] <> IN = TOP
//      | val x49 : DFUInt[8] <> IN = TOP
//      | val x50 : DFUInt[8] <> IN = TOP
//      | val x51 : DFUInt[8] <> IN = TOP
//      | val x52 : DFUInt[8] <> IN = TOP
//      | val x53 : DFUInt[8] <> IN = TOP
//      | val x54 : DFUInt[8] <> IN = TOP
//      | val x55 : DFUInt[8] <> IN = TOP
//      | val x56 : DFUInt[8] <> IN = TOP
//      | val x57 : DFUInt[8] <> IN = TOP
//      | val x58 : DFUInt[8] <> IN = TOP
//      | val x59 : DFUInt[8] <> IN = TOP
//      | val x60 : DFUInt[8] <> IN = TOP
//      | val x61 : DFUInt[8] <> IN = TOP
//      | val x62 : DFUInt[8] <> IN = TOP
//      | val x63 : DFUInt[8] <> IN = TOP
//      | val x64 : DFUInt[8] <> IN = TOP
//      | val x65 : DFUInt[8] <> IN = TOP
//      | val x66 : DFUInt[8] <> IN = TOP
//      | val x67 : DFUInt[8] <> IN = TOP
//      | val x68 : DFUInt[8] <> IN = TOP
//      | val x69 : DFUInt[8] <> IN = TOP
//      | val x70 : DFUInt[8] <> IN = TOP
//      | val x71 : DFUInt[8] <> IN = TOP
//      | val x72 : DFUInt[8] <> IN = TOP
//      | val x73 : DFUInt[8] <> IN = TOP
//      | val x74 : DFUInt[8] <> IN = TOP
//      | val x75 : DFUInt[8] <> IN = TOP
//      | val x76 : DFUInt[8] <> IN = TOP
//      | val x77 : DFUInt[8] <> IN = TOP
//      | val x78 : DFUInt[8] <> IN = TOP
//      | val x79 : DFUInt[8] <> IN = TOP
//      | val x80 : DFUInt[8] <> IN = TOP
//      | val x81 : DFUInt[8] <> IN = TOP
//      | val x82 : DFUInt[8] <> IN = TOP
//      | val x83 : DFUInt[8] <> IN = TOP
//      | val x84 : DFUInt[8] <> IN = TOP
//      | val x85 : DFUInt[8] <> IN = TOP
//      | val x86 : DFUInt[8] <> IN = TOP
//      | val x87 : DFUInt[8] <> IN = TOP
//      | val x88 : DFUInt[8] <> IN = TOP
//      | val x89 : DFUInt[8] <> IN = TOP
//      | val x90 : DFUInt[8] <> IN = TOP
//      | val x91 : DFUInt[8] <> IN = TOP
//      | val x92 : DFUInt[8] <> IN = TOP
//      | val x93 : DFUInt[8] <> IN = TOP
//      | val x94 : DFUInt[8] <> IN = TOP
//      | val x95 : DFUInt[8] <> IN = TOP
//      | val x96 : DFUInt[8] <> IN = TOP
//      | val x97 : DFUInt[8] <> IN = TOP
//      | val x98 : DFUInt[8] <> IN = TOP
//      | val x99 : DFUInt[8] <> IN = TOP
//      |      }
//    """)
//
//}
