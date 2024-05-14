package dfhdl

class ExitTrapSuite extends munit.FunSuite:
  sealed case class ExitException(status: Int)
      extends SecurityException(s"sys.exit($status) event discovered") {}
  sealed class NoExitSecurityManager extends SecurityManager:
    override def checkPermission(perm: java.security.Permission): Unit = {}
    override def checkPermission(perm: java.security.Permission, context: Object): Unit = {}
    override def checkExit(status: Int): Unit =
      super.checkExit(status)
      throw ExitException(status)

  override def beforeAll(): Unit = System.setSecurityManager(new NoExitSecurityManager())
  override def afterAll(): Unit = System.setSecurityManager(null)
end ExitTrapSuite
