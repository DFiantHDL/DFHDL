package dfhdl.compiler.stages

import dfhdl.compiler.ir
import dfhdl.compiler.ir.DFDesignBlock
import dfhdl.compiler.ir.DFDesignBlock.*
import dfhdl.internals.ClasspathResources
import dfhdl.options.CompilerOptions
import java.nio.file.{Files, Paths}

/** Commit-time handling of foreign IP blackboxes (see `EDBlackBox.ForeignIP`).
  *
  * DFHDL has no tool selection at commit time (and on Windows cannot tell apart the MSVC vs MinGW C
  * runtime needed), so commit is a dumb mirror: every classpath resource under a foreign IP's
  * `resourcePath` is copied, preserving its subfolder layout, into the same relative path in the
  * project (`resourcePath` is `dfhdl-ips/<dclName>`). The per-system / per-tool selection of which
  * binary to load and which HDL wrapper to compile happens later at simulate time (see
  * `Tool.foreignWrapperFiles` and the per-tool FFI flags).
  */
object ForeignResources:
  def commit(db: ir.DB, topCommitPathStr: String)(using co: CompilerOptions): ir.DB =
    val foreigns = db.members
      .collect { case d: DFDesignBlock => d.foreignIPSource }
      .flatten
      .distinctBy(_.resourcePath)
    val base = Paths.get(topCommitPathStr)
    foreigns.foreach { fsrc =>
      val ipDir = base.resolve(fsrc.resourcePath)
      val resources = ClasspathResources.listResources(fsrc.resourcePath)
      val prefix = fsrc.resourcePath.stripPrefix("/").stripSuffix("/") + "/"
      // mirror every resource (binaries for all systems + all HDL wrappers), preserving subpaths.
      // The release names files unversioned (the binaries' internal names — DLL export name, VPI
      // module name, ABI symbols — are never versioned, and the IP's `dpiLib`/`vpiModule`/`vhpiLib`
      // base names match), so they are copied verbatim.
      resources.foreach { res =>
        val rel = res.stripPrefix(prefix)
        val dest = ipDir.resolve(rel)
        Option(dest.getParent).foreach(Files.createDirectories(_))
        ClasspathResources.readBytes(res).foreach(bytes => Files.write(dest, bytes))
      }
    }
    db
  end commit
end ForeignResources
