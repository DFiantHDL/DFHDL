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
    defaultRstCfg: DefaultRstCfg,
    printDesignCodeBefore: PrintDesignCodeBefore,
    printDesignCodeAfter: PrintDesignCodeAfter,
    printGenFiles: PrintGenFiles
)
object CompilerOptions:
  given default(using
      commitFolder: CommitFolder = "sandbox",
      newFolderForTop: NewFolderForTop = true,
      backend: Backend = dfhdl.backends.verilog.sv2005,
      logLevel: CompilerLogLevel = LogLevel.WARN,
      defaultClkCfg: DefaultClkCfg = ClkCfg(ClkCfg.Edge.Rising),
      defaultRstCfg: DefaultRstCfg = RstCfg(RstCfg.Mode.Sync, RstCfg.Active.High),
      printDesignCodeBefore: PrintDesignCodeBefore = false,
      printDesignCodeAfter: PrintDesignCodeAfter = false,
      printGenFiles: PrintGenFiles = false
  ): CompilerOptions =
    CompilerOptions(
      commitFolder = commitFolder, newFolderForTop = newFolderForTop, backend = backend,
      logLevel = logLevel, defaultClkCfg = defaultClkCfg, defaultRstCfg = defaultRstCfg,
      printDesignCodeBefore = printDesignCodeBefore, printDesignCodeAfter = printDesignCodeAfter,
      printGenFiles = printGenFiles
    )

  extension (co: CompilerOptions)
    def topCommitPath(stagedDB: ir.DB): String =
      if (co.newFolderForTop) s"${co.commitFolder}$separatorChar${stagedDB.top.dclName}"
      else co.commitFolder
    def compilePath(stagedDB: ir.DB): String =
      s"${co.topCommitPath(stagedDB)}${separatorChar}hdl"

  opaque type CommitFolder <: String = String
  given Conversion[String, CommitFolder] = identity

  opaque type NewFolderForTop <: Boolean = Boolean
  given Conversion[Boolean, NewFolderForTop] = identity

  opaque type Backend <: BackendCompiler = BackendCompiler
  given Conversion[BackendCompiler, Backend] = identity

  opaque type CompilerLogLevel <: LogLevel = LogLevel
  given Conversion[LogLevel, CompilerLogLevel] = identity

  opaque type DefaultClkCfg <: ClkCfg = ClkCfg
  given Conversion[ClkCfg, DefaultClkCfg] = identity
  given Conversion[None.type, DefaultClkCfg] = x => x.asInstanceOf[DefaultClkCfg]

  opaque type DefaultRstCfg <: RstCfg = RstCfg
  given Conversion[RstCfg, DefaultRstCfg] = identity
  given Conversion[None.type, DefaultRstCfg] = x => x.asInstanceOf[DefaultRstCfg]

  opaque type PrintDesignCodeBefore <: Boolean = Boolean
  given Conversion[Boolean, PrintDesignCodeBefore] = identity

  opaque type PrintDesignCodeAfter <: Boolean = Boolean
  given Conversion[Boolean, PrintDesignCodeAfter] = identity

  opaque type PrintGenFiles <: Boolean = Boolean
  given Conversion[Boolean, PrintGenFiles] = identity
end CompilerOptions
