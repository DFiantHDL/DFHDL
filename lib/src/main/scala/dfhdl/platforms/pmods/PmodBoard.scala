package dfhdl.platforms.pmods
import dfhdl.platforms.Board

trait PmodBoard extends Board:
  val pmodConn: PmodConn.Male

trait PmodDualBoard extends Board:
  val pmodDualConn: PmodDualConn.Male
