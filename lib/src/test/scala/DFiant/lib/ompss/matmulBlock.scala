import DFiant._
import lib.ompss._
import sim.DFSimDesign

import scala.util.Random
import matmulBlock._
import lib.sequential._

/**
  * The Matrix Multiplication Interface
  */
@df class matmulBlock_ifc extends OmpssIfc {
  final val a = OmpssARR(SIZE, 4) <> IN
  final val b = OmpssARR(SIZE, BSIZE / 4) <> IN
  final val c = OmpssARR(SIZE, BSIZE / 2) <> INOUT
}

/**
  * The Matrix Multiplication Design
  */
@df class matmulBlock extends DFDesign {
  final val io = new matmulBlock_ifc
  import io._
  final val cntBSize = DFUInt.until(BSIZE) <> VAR init 0
  final val cntHBSize = cntBSize.resize(cntBSize.width-1)
  final val cntQBSize = cntHBSize.resize(cntHBSize.width-1)
  final val cnt32x8 = DFUInt.until(32) <> VAR init 0
  final val cnt16xcnt32x8 = DFUInt.until(BSIZE) <> VAR init 0
  final val aData = DFBits(32) <> VAR := ?
  final val bData = DFBits(32) <> VAR := ?
  final val cData = DFBits(32) <> VAR := ?
  final val dataValid = DFBit <> VAR init 0 := 0
  final val cWriteDone = DFBit <> VAR init 0 := 0

  //it's useful to disable by default, and create temporary changes within the FSM
  a.disable()
  b.disable()
  c.disable(dontCareAddress = false)

  val fsm_suspended : FSM = ap.startFSM =^>{
    cntBSize := 0
    cnt32x8 := 0
    cnt16xcnt32x8 := 0
  } ==> fsm_address
  val fsm_address : FSM = {
    a.setAddress(cnt32x8.bits(4,1) ++ cnt16xcnt32x8.bits(3, 2))
    b.setAddress(cnt16xcnt32x8.bits ++ cntBSize.bits(3, 2))
    c.setAddress(cnt32x8.bits)
  } ==> fsm_read
  val fsm_read : FSM = FSM {
    aData := a.getDataRead(cnt16xcnt32x8.resize(2))
    bData := b.getDataRead(cntQBSize)
    cData := c.getDataRead(cntHBSize)
    dataValid := 1

    cntBSize := cntBSize.prev + 1
    ifdf(cntHBSize == 7) {
      cnt32x8 := cnt32x8.prev + 1
      fsm_write.goto()
      ifdf(cnt32x8.prev == 31) {
        cnt16xcnt32x8 := cnt16xcnt32x8.prev + 1
        ifdf (cnt16xcnt32x8.prev == 15 && cntBSize.prev == 15) {
          fsm_finish.goto()
        }
      }
    }.elseifdf(cntQBSize == 3) {
      fsm_address.goto()
    }
  }
  val fsm_write: FSM  = waitUntil(cWriteDone.prev) ==> fsm_address
  val fsm_finish: FSM = ap.finishFSM(cWriteDone.prev) ==> fsm_suspended

  val result =
    (cData.uint.pipe(1) + (aData.uint * bData.uint).pipe(1)).pipe(1).bits
  val resultValid = dataValid.pipe(2)

  val write_loop: FSM = doFor(0 until 8, resultValid) { cPartSelWrite =>
    ifdf(resultValid) {
      c.write(cPartSelWrite, result)
    }
  } =^> { cWriteDone := 1 } ==> write_loop
}


//@df class matmulBlock4 extends DFDesign {
//  final val io = new matmulBlock_ifc
//  import io._
//  final val cnt16 = DFUInt.until(BSIZE) <> VAR init 0
//  final val cnt8 = cnt16.resize(3)
//  final val cnt4 = cnt8.resize(2)
//  final val cnt32x8 = DFUInt.until(32) <> VAR init 0
//  final val cnt16xcnt32x8 = DFUInt.until(BSIZE) <> VAR init 0
//  final val aData = Vector.fill(4)(DFBits(32) <> VAR := ?)
//  final val bData = Vector.fill(4)(DFBits(32) <> VAR := ?)
//  final val cData = DFBits(32) <> VAR := ?
//  final val dataValid = DFBit <> VAR init 0 := 0
//  final val cWriteDone = DFBit <> VAR init 0 := 0
//
//  //it's useful to disable by default, and create temporary changes within the FSM
//  a.disable()
//  b.disable()
//  c.disable(dontCareAddress = false)
//
//  val fsm_suspended : FSM = ap.startFSM =^>{
//    cnt16 := 0
//    cnt32x8 := 0
//    cnt16xcnt32x8 := 0
//  } ==> fsm_address
//  val fsm_address : FSM = {
//    a.setAddress(cnt32x8.bits(4,1) ++ cnt16xcnt32x8.bits(3, 2))
//    b.setAddress(cnt16xcnt32x8.bits ++ cnt16.bits(3, 2))
//    c.setAddress(cnt32x8.bits)
//  } ==> fsm_read
//  val fsm_read : FSM = FSM {
//    for (i <- 0 until 4) {
//      aData(i) := a.partitions(i).q
//      bData(i) := b.partitions(i).q
//    }
//    cData := c.getDataRead(cnt8)
//    dataValid := 1
//
//    cnt16 := cnt16.prev + 1
//    ifdf(cnt8 == 7) {
//      cnt32x8 := cnt32x8.prev + 1
//      fsm_write.goto()
//      ifdf (cnt32x8.prev == 31) {
//        cnt16xcnt32x8 := cnt16xcnt32x8.prev + 1
//        ifdf (cnt16xcnt32x8.prev == 15 && cnt16.prev == 15) {
//          fsm_finish.goto()
//        }
//      }
//    }.elseifdf(cnt4 == 3) {
//      fsm_address.goto()
//    }
//  }
//  val fsm_write : FSM = waitUntil(cWriteDone.prev) ==> fsm_address
//  val fsm_finish : FSM = ap.finishFSM(cWriteDone.prev) ==> fsm_suspended
//
//  val result = (cData.uint.pipe(1) + (aData.uint * bData.uint).pipe(1)).pipe(1).bits
//  val resultValid = dataValid.pipe(2)
//
//  val write_loop : FSM = doFor(0 until 8, resultValid) {cPartSelWrite =>
//    ifdf (resultValid) {
//      c.write(cPartSelWrite, result)
//    }
//  } =^> {cWriteDone := 1} ==> write_loop
//}

/**
  * Holds the BSIZE constant, and various simulation-related oper
  */
object matmulBlock {
  final val BSIZE = 16
  final val SIZE = BSIZE * BSIZE
  val debugReport : Boolean = false
  /**
    * Matrix for simulation
    */
  type Matrix = Vector[Vector[Int]]
  object Matrix {
    def genRandomFixedSeed(seed: Long): Matrix = {
      Random.setSeed(seed)
      Vector.fill(BSIZE)(Vector.fill(BSIZE)(Random.nextInt(100)))
    }
    def genRandom: Matrix =
      Vector.fill(BSIZE)(Vector.fill(BSIZE)(Random.nextInt(100)))
    def genOnes: Matrix =
      Vector.fill(BSIZE)(Vector.fill(BSIZE)(1))
    def genIdentity : Matrix =
      Vector.tabulate(BSIZE)(i => Vector.tabulate(BSIZE)(j => if (i == j) 1 else 0))
    def genHexCount : Matrix =
      Vector.tabulate(BSIZE)(i => Vector.tabulate(BSIZE)(j => j * BSIZE + i))
  }
  implicit class MatrixOps(matrix: Matrix) {
    def *(that: Matrix): Matrix = {
      for (row <- matrix)
        yield for (col <- that.transpose)
          yield row zip col map Function.tupled(_ * _) reduceLeft (_ + _)
    }
    def getPartition(factor: Int, idx: Int): Vector[DFBits.Token] =
      matrix.view.flatten.zipWithIndex.collect {
        case i if i._2 % factor == idx => DFBits.Token(32, i._1)
      }.toVector
  }
}

/**
  * Matrix Block Simulation Driver
  */
@df class matmulBlockDriver extends DFSimDesign {
  final val io = new matmulBlock_ifc <> FLIP
  import io._
  import lib.sequential._
  private val resultOK = DFBool <> VAR := true
  private val ap_drv_fsm : FSM =
    {
      ap.start := 0
    } ==> {
      ap.start := 1
    } ==> waitUntil(ap.ready) ==> {
      sim.report(msg"Got first ap_ready")
    } ==> waitForever()


  val cycle = DFUInt(32) <> VAR init 0
  cycle := cycle + 1
  println("a:")
  private val aMatrix = Matrix.genRandomFixedSeed(15L)
  println(aMatrix.mkString("\n"))
  println("b:")
  private val bMatrix = Matrix.genRandomFixedSeed(29L)
  println(bMatrix.mkString("\n"))
  println("c:")
  private val cMatrix = aMatrix * bMatrix
  println(cMatrix.mkString("\n"))
  sim.assert(
    a.partitions.map(_.address).allAreEqual,
    msg"unexpected partition mismatch",
    sim.Failure
  )
  sim.assert(
    a.partitions.map(_.ce).allAreEqual,
    msg"unexpected partition mismatch",
    sim.Failure
  )
  sim.assert(
    b.partitions.map(_.address).allAreEqual,
    msg"unexpected partition mismatch",
    sim.Failure
  )
  sim.assert(
    b.partitions.map(_.ce).allAreEqual,
    msg"unexpected partition mismatch",
    sim.Failure
  )
  sim.assert(
    c.partitions.map(_.address).allAreEqual,
    msg"unexpected partition mismatch",
    sim.Failure
  )

  private val aDebug = a.partitions.map { p =>
    val aMemArr = DFBits(32).X(SIZE / p.factor) <> VAR init aMatrix.getPartition(p.factor, p.idx)
    aMemArr := aMemArr.prev
    val q = DFBits(32) <> VAR init b0s
    p.q := q.prev
    ifdf(p.ce) {
      q := aMemArr(p.address.uint)
    }
    (q.uint, p)
  }
  if (debugReport) {
    ifdf (aDebug.head._2.ce) {
      sim.report(msg"    $cycle    rd    a[${aDebug.head._2.address.uint}]    ${aDebug(0)._1} ${aDebug(1)._1} ${aDebug(2)._1} ${aDebug(3)._1}")
    }
  }

  private val bDebug = b.partitions.map { p =>
    val bMemArr = DFBits(32).X(SIZE / p.factor) <> VAR init bMatrix.getPartition(p.factor, p.idx)
    bMemArr := bMemArr.prev
    val q = DFBits(32) <> VAR init b0s
    p.q := q.prev
    ifdf(p.ce) {
      q := bMemArr(p.address.uint)
    }
    (q.uint, p)
  }
  if (debugReport) {
    ifdf (bDebug.head._2.ce) {
      sim.report(msg"    $cycle    rd    b[${bDebug.head._2.address.uint}]    ${bDebug(0)._1} ${bDebug(1)._1} ${bDebug(2)._1} ${bDebug(3)._1}")
    }
  }

  val cDebug = c.partitions.map { p =>
    val cMemArr = DFBits(32).X(SIZE / p.factor) <> VAR init Vector.fill(32)(b0s)
    cMemArr := cMemArr.prev
    val q = DFBits(32) <> VAR init b0s
    p.q := q.prev
    ifdf(p.ce) {
      ifdf(p.we) {
        if (debugReport)
          sim.report(msg"    $cycle    wr    c_${p.idx}[${p.address.uint}]    ${p.d.uint}")
        cMemArr(p.address.uint) := p.d
      }
      q := cMemArr(p.address.uint)
    }
    val cMemArrRef = DFBits(32).X(SIZE / p.factor) <> VAR init cMatrix.getPartition(p.factor, p.idx)
    cMemArrRef := cMemArrRef.prev

    ifdf(ap.ready) {
      ifdf(cMemArr =!= cMemArrRef) {
        resultOK := false
      }
      cMemArr := Vector.fill(32)(b0s)
    }
    (q.uint, p)
  }
  ifdf (cDebug.head._2.ce && !cDebug.head._2.we) {
    if (debugReport)
      sim.report(msg"    $cycle    rd    c[${cDebug.head._2.address.uint}]    ${cDebug(0)._1} ${cDebug(1)._1} ${cDebug(2)._1} ${cDebug(3)._1} ${cDebug(4)._1} ${cDebug(5)._1} ${cDebug(6)._1} ${cDebug(7)._1}")
  }

  ifdf(ap.ready) {
    ifdf(resultOK) {
      nextStep.goto()
      resultOK := true
      sim.report(msg"Finished matrix mul successfully", sim.Note)
    }.elsedf {
      sim.report(msg"Bad matrix mul", sim.Warning)
      sim.finish()
    }
  } ==> ifdf(ap.ready) {
    ifdf(resultOK) {
      sim.report(msg"Finished matrix mul successfully", sim.Note)
    }.elsedf {
      sim.report(msg"Bad matrix mul", sim.Warning)
    }
    sim.finish()
  }
}

/**
  * Matrix Block Tester (connects the driver and DUT)
  */
@df class matmulBlockTest extends DFSimDesign {
  final val dut = new matmulBlock
  final val drv = new matmulBlockDriver
  dut.io <> drv.io
}

/**
  * Matrix Block Tester Application Entry Point
  */
object matmulBlockTestApp extends App {
  val top_test = new matmulBlockTest
  import sim.tools.modelsim
  top_test.printCodeString
    .compile.printCodeString.toFolder("matmulBlock")
//    .removeFiles(_.contains("matmulBlock.vhdl"))
//    .addExternalFiles(
//      "matmulBlock/matmulBlock_hls_abkb.vhdl",
//      "matmulBlock/matmulBlock_hls_acud.vhdl",
//      "matmulBlock/matmulBlock_hls_adEe.vhdl",
//      "matmulBlock/matmulBlock_hls_aeOg.vhdl",
//      "matmulBlock/matmulBlock_moved.vhdl",
//    )
    .simulation.run()
}

/**
  * Matrix Block Build Application Entry Point
  */
object matmulBlockApp extends App {
  val top = new matmulBlock
  top.printCodeString.compile.printCodeString.toFile("matmulBlock/zedboard/matmul_ait/xilinx/HLS/matmulBlock/solution1/impl/ip/hdl/vhdl/matmulBlock_moved.vhd")
}
