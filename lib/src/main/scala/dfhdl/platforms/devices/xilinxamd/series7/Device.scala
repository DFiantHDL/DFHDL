package dfhdl.platforms.devices.xilinxamd.series7
import dfhdl.internals.HasTypeName
import dfhdl.hw.constraints.*

trait Device extends dfhdl.platforms.devices.Device:
  final lazy val vendor: deviceID.Vendor = deviceID.Vendor.XilinxAMD
  final lazy val deviceVersion: String = ""
