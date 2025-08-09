package dfhdl.platforms.devices
import dfhdl.platforms.Platform
import dfhdl.hw.constraints.*

trait Device extends Platform, dfhdl.platforms.devices.Package:
  lazy val vendor: deviceID.Vendor
  lazy val deviceName: String
  val speedGrade: Int
  injectConstraint(deviceID(vendor, deviceName, packageName, speedGrade).asIR)
