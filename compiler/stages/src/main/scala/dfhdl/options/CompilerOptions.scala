package dfhdl.options
import dfhdl.compiler.ir
import dfhdl.compiler.stages.BackendCompiler
import dfhdl.core.{ClkCfg, RstCfg}

import java.io.File.separatorChar

import CompilerOptions.*
final case class CompilerOptions(
    commitFolder: CommitFolder,
    newFolderForTop: NewFolderForTop,
    backend: Backend,
    logLevel: LogLevel,
    defaultClkCfg: DefaultClkCfg,
    defaultRstCfg: DefaultRstCfg,
    printDesignCodeBefore: PrintDesignCodeBefore,
    printDesignCodeAfter: PrintDesignCodeAfter,
    printGenFiles: PrintGenFiles
)
object CompilerOptions:
  given default(using
      commitFolder: CommitFolder,
      newFolderForTop: NewFolderForTop,
      backend: Backend,
      logLevel: LogLevel,
      defaultClkCfg: DefaultClkCfg = ClkCfg(ClkCfg.Edge.Rising),
      defaultRstCfg: DefaultRstCfg = RstCfg(RstCfg.Mode.Sync, RstCfg.Active.High),
      printDesignCodeBefore: PrintDesignCodeBefore,
      printDesignCodeAfter: PrintDesignCodeAfter,
      printGenFiles: PrintGenFiles
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
  object CommitFolder:
    given CommitFolder = "sandbox"
    given Conversion[String, CommitFolder] = identity

  opaque type NewFolderForTop <: Boolean = Boolean
  object NewFolderForTop:
    given NewFolderForTop = true
    given Conversion[Boolean, NewFolderForTop] = identity

  opaque type Backend <: BackendCompiler = BackendCompiler
  object Backend:
    given Backend = dfhdl.backends.verilog.sv2005
    given Conversion[BackendCompiler, Backend] = identity
    export dfhdl.backends.*

  opaque type LogLevel <: dfhdl.options.LogLevel = dfhdl.options.LogLevel
  given Conversion[wvlet.log.LogLevel, LogLevel] = x => x.asInstanceOf[LogLevel]
  object LogLevel:
    given (using logLevel: dfhdl.options.LogLevel): LogLevel = logLevel
    export dfhdl.options.LogLevel.*

  opaque type DefaultClkCfg <: ClkCfg = ClkCfg
  given Conversion[ClkCfg, DefaultClkCfg] = identity
  given Conversion[None.type, DefaultClkCfg] = x => x.asInstanceOf[DefaultClkCfg]

  opaque type DefaultRstCfg <: RstCfg = RstCfg
  given Conversion[RstCfg, DefaultRstCfg] = identity
  given Conversion[None.type, DefaultRstCfg] = x => x.asInstanceOf[DefaultRstCfg]

  opaque type PrintDesignCodeBefore <: Boolean = Boolean
  object PrintDesignCodeBefore:
    given PrintDesignCodeBefore = false
    given Conversion[Boolean, PrintDesignCodeBefore] = identity

  opaque type PrintDesignCodeAfter <: Boolean = Boolean
  object PrintDesignCodeAfter:
    given PrintDesignCodeAfter = false
    given Conversion[Boolean, PrintDesignCodeAfter] = identity

  opaque type PrintGenFiles <: Boolean = Boolean
  object PrintGenFiles:
    given PrintGenFiles = false
    given Conversion[Boolean, PrintGenFiles] = identity
end CompilerOptions
