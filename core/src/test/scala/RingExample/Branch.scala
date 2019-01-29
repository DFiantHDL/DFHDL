//package RingExample
//
//abstract class Branch(nodesNum : Int, nodeID : Int) {
//  Branch =>
//  //IO ports
//  val inFlit                  : DFFlit#IN
//  val outFlit                 : DFFlit#OUT
//  val localCtrlFlitFromBranch : DFFlit#OUT
//  val localDataFlitFromBranch : DFFlit#OUT
//  val localCtrlFlitToBranch   : DFFlit#IN
//  val localDataFlitToBranch   : DFFlit#IN
//
//  private val remoteCtrlFlit  = DFFlit(nodesNum)
//  private val remoteDataFlit  = DFFlit(nodesNum)
//
//  new ArbIngress(nodesNum, nodeID) {
//    val inFlit                : DFFlit#IN  = Branch.inFlit
//    val localCtrlFlit         : DFFlit#OUT = Branch.localCtrlFlitFromBranch
//    val localDataFlit         : DFFlit#OUT = Branch.localDataFlitFromBranch
//    val remoteCtrlFlit        : DFFlit#OUT = Branch.remoteCtrlFlit
//    val remoteDataFlit        : DFFlit#OUT = Branch.remoteDataFlit
//  }
//
//  new ArbEgress(nodesNum, nodeID) {
//    val outFlit               : DFFlit#OUT = Branch.outFlit
//    val localCtrlFlit         : DFFlit#IN  = Branch.localCtrlFlitToBranch
//    val localDataFlit         : DFFlit#IN  = Branch.localDataFlitToBranch
//    val remoteCtrlFlit        : DFFlit#IN  = Branch.remoteCtrlFlit
//    val remoteDataFlit        : DFFlit#IN  = Branch.remoteDataFlit
//  }
//}
