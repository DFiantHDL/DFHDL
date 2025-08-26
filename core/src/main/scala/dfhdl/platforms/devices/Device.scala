package dfhdl.platforms.devices
import dfhdl.platforms.Platform
import dfhdl.hw.constraints.*

trait Device extends Platform, dfhdl.platforms.devices.Package:
  lazy val vendor: deviceID.Vendor
  lazy val deviceName: String
  lazy val partName: String
  lazy val deviceVersion: String
  injectConstraint(deviceID(vendor, deviceName, partName, deviceVersion).asIR)
