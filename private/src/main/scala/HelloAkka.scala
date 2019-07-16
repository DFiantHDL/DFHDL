//import akka.actor._
//import akka.stream._
//object HelloAkka extends App {
//  implicit val system = ActorSystem("QuickStart")
//  implicit val materializer = ActorMaterializer()
//
//  val g = RunnableGraph.fromGraph(GraphDSL.create() { implicit builder: GraphDSL.Builder[NotUsed] =>
//    import GraphDSL.Implicits._
//    val in = Source(1 to 10)
//    val out = Sink.foreach(println)
//
//    val bcast :  UniformFanOutShape[Int, Int] = builder.add(Broadcast[Int](2))
//    val merge : FanInShape2[Int, Int, Int] = builder.add(ZipWith[Int, Int, Int]((a, b) => a + b))
//
//    in ~> bcast ~> merge.in0
//    bcast ~> merge.in1
//    merge.out ~> out
//    ClosedShape
//  })
//
////  import GraphDSL.Implicits._
////  val e = RunnableGraph.fromGraph(GraphDSL.create() { implicit builder =>
////    val A: Outlet[Int]                  = builder.add(Source.single(0)).out
////    val B: UniformFanOutShape[Int, Int] = builder.add(Broadcast[Int](2))
////    val C: UniformFanInShape[Int, Int]  = builder.add(ZipN[Int](2))
////    val D: FlowShape[Int, Int]          = builder.add(Flow[Int].map(_ + 1))
////    val E: UniformFanOutShape[Int, Int] = builder.add(Balance[Int](2))
////    val F: UniformFanInShape[Int, Int]  = builder.add(Merge[Int](2))
////    val G: Inlet[Any]                   = builder.add(Sink.foreach(println)).in
////
////    C     <~      F
////    A  ~>  B  ~>  C     ~>      F
////    B  ~>  D  ~>  E  ~>  F
////    E  ~>  G
////
////    ClosedShape
////  })
//
//  g.run()
//}
