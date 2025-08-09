package dfhdl.platforms.devices
import dfhdl.platforms.Platform
import dfhdl.hw.constraints.*

trait Device extends Platform, dfhdl.platforms.devices.Package:
  lazy val vendor: deviceID.Vendor
  lazy val deviceName: String
  lazy val deviceVersion: String
  val speedGrade: String
  injectConstraint(deviceID(vendor, deviceName, packageName, speedGrade, deviceVersion).asIR)
