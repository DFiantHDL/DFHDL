package dfhdl.platforms.pmods
import dfhdl.platforms.Board
import dfhdl.platforms.resources.*
import dfhdl.DFC
import Resource.CanConnect

trait PmodBoard extends Board, Resource:
  protected val pmodConn: PmodConn.Male
object PmodBoard:
  given [B <: PmodBoard, R <: Resource](
      using CanConnect[PmodConn.Male, R]
  ): CanConnect[B, R] with
    def connect(board: B, resource: R)(using DFC): Unit = board.pmodConn <> resource

trait PmodDualBoard extends Board, Resource:
  protected val pmodDualConn: PmodDualConn.Male
object PmodDualBoard:
  given [B <: PmodDualBoard, R <: Resource](using
      CanConnect[PmodDualConn.Male, R]
  ): CanConnect[B, R] with
    def connect(board: B, resource: R)(using DFC): Unit = board.pmodDualConn <> resource
