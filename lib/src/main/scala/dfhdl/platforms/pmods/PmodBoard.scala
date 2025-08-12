package dfhdl.platforms.pmods
import dfhdl.platforms.Board
import dfhdl.platforms.resources.*
import Resource.CanConnect

trait PmodBoard extends Board, Resource:
  protected val pmodConn: PmodConn.Male
object PmodBoard:
  given [B <: PmodBoard, FC <: PmodConn.Female]: CanConnect[B, FC] = (b, fc) =>
    b.pmodConn <> fc

trait PmodDualBoard extends Board, Resource:
  protected val pmodDualConn: PmodDualConn.Male
object PmodDualBoard:
  given [B <: PmodDualBoard, FC <: PmodDualConn.Female]: CanConnect[B, FC] = (b, fc) =>
    b.pmodDualConn <> fc
