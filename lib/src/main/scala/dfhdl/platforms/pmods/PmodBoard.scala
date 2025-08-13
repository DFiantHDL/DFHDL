package dfhdl.platforms.pmods
import dfhdl.platforms.Board
import dfhdl.platforms.resources.*
import Resource.CanConnect

trait PmodBoard extends Board, Resource:
  protected val pmodConn: PmodConn.Male
object PmodBoard:
  given [B <: PmodBoard, R <: Resource](
      using CanConnect[PmodConn.Male, R]
  ): CanConnect[B, R] = (b, r) => b.pmodConn <> r

trait PmodDualBoard extends Board, Resource:
  protected val pmodDualConn: PmodDualConn.Male
object PmodDualBoard:
  given [B <: PmodDualBoard, R <: Resource](using
      CanConnect[PmodDualConn.Male, R]
  ): CanConnect[B, R] = (b, r) => b.pmodDualConn <> r
