package dfhdl.platforms.devices
import dfhdl.platforms.Platform
import dfhdl.compiler.ir.constraints.DeviceID

trait Device extends Platform, dfhdl.platforms.devices.Package:
  lazy val vendor: DeviceID.Vendor
  lazy val deviceName: String
  lazy val partName: String
  lazy val deviceVersion: String
  injectConstraint(DeviceID(vendor, deviceName, partName, deviceVersion))
