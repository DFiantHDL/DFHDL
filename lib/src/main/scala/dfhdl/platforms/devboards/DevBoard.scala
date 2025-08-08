package dfhdl.platforms.devboards
import dfhdl.platforms.Board
import dfhdl.platforms.devices.Device

trait DevBoard extends Board:
  val device: Device
