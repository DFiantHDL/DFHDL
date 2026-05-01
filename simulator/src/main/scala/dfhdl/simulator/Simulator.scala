package dfhdl.simulator

import dfhdl.compiler.ir.*
import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.stream.*
import org.apache.pekko.stream.scaladsl.*
import org.apache.pekko.{Done, NotUsed}
import scala.concurrent.{Future, ExecutionContext}

/** Simulator that maps DFHDL designs to Pekko Streams */
class Simulator(db: DB)(using system: ActorSystem, ec: ExecutionContext):
  import db.getSet

  /** Maps a DFHDL design to a Pekko Stream */
  def simulateDesign(design: DFDesignBlock): Source[Map[String, Any], NotUsed] =
    // Walk members directly:
    val designPorts = db.members.view.collect {
      case dcl: DFVal.Dcl if dcl.isPort && dcl.getOwnerDesign == design =>
        dcl.getName -> dcl
    }.toList
    // Create a source for each input port
    val inputPorts = designPorts.filter { case (_, port) => port.isPortIn }

    // Create a flow for each process block
    val processFlows = db.members.collect {
      case block: DFBlock if block.getOwnerDesign == design =>
        createProcessFlow(block)
    }

    // Combine all flows
    val combinedFlow = processFlows.reduceLeft(_ via _)

    // Create a sink for each output port
    val outputPorts = designPorts.filter { case (_, port) => port.isPortOut }

    // Create the final stream
    Source.fromGraph(GraphDSL.create() { implicit builder =>
      import GraphDSL.Implicits.*

      // Add input ports as sources
      val inputs = inputPorts.map { case (name, port) =>
        name -> builder.add(Source.single(port.getConstData[Any].getOrElse(0)))
      }.toMap

      // Add process blocks as flows
      val processes = processFlows.map(flow => builder.add(flow))

      // Add output ports as sinks
      val outputs = outputPorts.map { case (name, port) =>
        name -> builder.add(Sink.ignore)
      }.toMap

      // Connect the graph
      // TODO: Implement proper connections based on DFHDL design

      SourceShape(builder.add(Source.empty[Map[String, Any]]).out)
    })
  end simulateDesign

  /** Creates a flow for a process block */
  private def createProcessFlow(block: DFBlock): Flow[Map[String, Any], Map[String, Any], NotUsed] =
    Flow[Map[String, Any]].map { inputs =>
      // TODO: Implement process block logic
      // This should evaluate the block's logic and return updated values
      inputs
    }

  /** Runs the simulation and returns a Future that completes when done */
  def run(design: DFDesignBlock): Future[Done] =
    simulateDesign(design)
      .runWith(Sink.ignore)
end Simulator
