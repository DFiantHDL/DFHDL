package dfhdl.options
import CompilerOptions.*
import dfhdl.compiler.ir
import dfhdl.compiler.stages.BackendCompiler
import dfhdl.core.{ClkCfg, RstCfg}
export wvlet.log.LogLevel

import java.io.File.separatorChar

final case class CompilerOptions(
    commitFolder: CommitFolder,
    newFolderForTop: NewFolderForTop,
    backend: Backend,
    logLevel: CompilerLogLevel,
    defaultClkCfg: DefaultClkCfg,
    defaultRstCfg: DefaultRstCfg
)
object CompilerOptions:
  given default(using
      commitFolder: CommitFolder = "sandbox",
      newFolderForTop: NewFolderForTop = true,
      backend: Backend = dfhdl.backends.verilog.sv2005,
      logLevel: CompilerLogLevel = LogLevel.WARN,
      defaultClkCfg: DefaultClkCfg = ClkCfg(ClkCfg.Edge.Rising),
      defaultRstCfg: DefaultRstCfg = RstCfg(RstCfg.Mode.Sync, RstCfg.Active.High)
  ): CompilerOptions =
    CompilerOptions(
      commitFolder = commitFolder, newFolderForTop = newFolderForTop, backend = backend,
      logLevel = logLevel, defaultClkCfg = defaultClkCfg, defaultRstCfg = defaultRstCfg
    )

  extension (co: CompilerOptions)
    def topCommitPath(stagedDB: ir.DB): String =
      if (co.newFolderForTop) s"${co.commitFolder}$separatorChar${stagedDB.top.dclName}"
      else co.commitFolder
    def compilePath(stagedDB: ir.DB): String =
      s"${co.topCommitPath(stagedDB)}${separatorChar}hdl"

  opaque type CommitFolder <: String = String
  given Conversion[String, CommitFolder] = x => x

  opaque type NewFolderForTop <: Boolean = Boolean
  given Conversion[Boolean, NewFolderForTop] = x => x

  opaque type Backend <: BackendCompiler = BackendCompiler
  given Conversion[BackendCompiler, Backend] = x => x

  opaque type CompilerLogLevel <: LogLevel = LogLevel
  given Conversion[LogLevel, CompilerLogLevel] = x => x

  opaque type DefaultClkCfg <: ClkCfg = ClkCfg
  given Conversion[ClkCfg, DefaultClkCfg] = x => x
  given Conversion[None.type, DefaultClkCfg] = x => x.asInstanceOf[DefaultClkCfg]

  opaque type DefaultRstCfg <: RstCfg = RstCfg
  given Conversion[RstCfg, DefaultRstCfg] = x => x
  given Conversion[None.type, DefaultRstCfg] = x => x.asInstanceOf[DefaultRstCfg]
end CompilerOptions
