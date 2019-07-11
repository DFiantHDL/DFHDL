//package DFiant.simulator
//import DFiant._
//import internals._
//
//import scala.collection.mutable
//import scala.collection.mutable.ListBuffer
//
//protected sealed trait TokenState
//object TokenState {
//  sealed class NotReady extends TokenState
//  case class NoToken() extends NotReady
//  case class Consumed(tokenOrError : TokenOrError) extends NotReady
//  sealed trait Ready extends TokenState {
//    val tokenOrError : TokenOrError
//  }
//  case class ReadyNew(tokenOrError : TokenOrError) extends Ready
//  case class ReadyUnchanged(tokenOrError : TokenOrError) extends Ready
//}
//
//abstract class TokenFIFO(slots : Int) {
//  private val queue : mutable.Queue[TokenOrError] = mutable.Queue.empty[TokenOrError]
//  def isEmpty : Boolean = queue.isEmpty
//  def isFull : Boolean = queue.length == slots
//  object enqueue {
//    def apply(token : DFAny.Token) : Unit = {
//
//    }
//    var ready : () => Boolean = () => true
//    def valid : Boolean = ???
//    def data : TokenOrError = ???
//    object sampled {
//      var valid : Boolean = false
//      var data : TokenOrError = Right("Uninitialized")
//    }
//    def update() : Unit = {
//      if (ready() && valid && !sampled.valid) sampled.data = data
//      sampled.valid = valid
//
//    }
//  }
//  object dequeue {
//    def ready : Boolean = ???
//    var valid : Boolean = false
//    var data : TokenOrError = Right("Uninitialized")
//    object sampled {
//      var ready : Boolean = false
//    }
//  }
//}
//
//abstract class TokenStream(dfVal : DFAny)(producers : List[DFAny], postJoinFunc : List[DFAny.Token] => DFAny.Token)(implicit sim : DFSimulator) {
//  object events {
//    private[TokenStream] var onAProducerReady : Option[DFAny => Unit] = None
//    private[TokenStream] var onAllProducersReady : Option[() => Unit] = None
//    def hookOnAProducerReady(func : DFAny => Unit) : Unit = onAProducerReady = Some(func)
//    def hookOnAllProducersReady(func : => Unit) : Unit = onAllProducersReady = Some(() => func)
//  }
//  private object history {
//    private val queue : mutable.Queue[TokenOrError] = mutable.Queue.empty[TokenOrError]
//  }
//  private val queue : mutable.Queue[TokenOrError] = mutable.Queue.empty[TokenOrError]
//  private val consumers : mutable.Set[DFAny] = mutable.Set.empty[DFAny]
//  final protected def addConsumer(consumer : DFAny) : Unit = consumers += consumer
//  final val producerStreams : List[TokenStream] = producers.map(p => sim.getTokenStream(p))
//  final lazy val consumerStreams : List[TokenStream] = consumers.toList.map(p => sim.getTokenStream(p))
//  private var tokenState : TokenState = TokenState.NoToken()
//
////  protected def join :
//  def getTokenState : TokenState = {
//    val xs = producerStreams.map(p => p.getTokenState)
//    xs collectFirst {case x : TokenState.NotReady => TokenState.NoToken()} getOrElse {
//      val toes = xs.collect{case x : TokenState.Ready => x.tokenOrError}
//      toes collectFirst {case Right(msg) => TokenState.ReadyNew(Right(msg))} getOrElse
//        TokenState.ReadyNew(Left(postJoinFunc(toes.collect{case Left(token) => token})))
//    }
//  }
//  sim.addTokenStream(dfVal, this)
//  producerStreams.foreach(p => p.addConsumer(dfVal))
//}
//object TokenStream {
//  case class Const(dfVal : DFAny.Const[_ <: DFAny])(implicit sim : DFSimulator) extends TokenStream(dfVal)(List(), tokens => dfVal.constLB.get)
//}