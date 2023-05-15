package dfhdl.compiler.stages

import dfhdl.compiler.printing.Printer
import dfhdl.core.Design
import dfhdl.options.{PrinterOptions, CommitOptions}
import dfhdl.compiler.ir
import java.nio.file.{Paths, Files}

opaque type CompiledDesign[D <: Design] = StagedDesign[D]
object CompiledDesign:
  def apply[D <: Design](sd: StagedDesign[D]): CompiledDesign[D] = sd
  extension [D <: Design](cd: CompiledDesign[D])
    def staged: StagedDesign[D] = cd
    def stagedDB: ir.DB = staged.stagedDB
    def toFolder(path: String = cd.stagedDB.top.dclName): CommittedDesign[D] =
      CommittedDesign(staged.newStage(Printer.commit(staged.stagedDB, path)))
    def commit(using co: CommitOptions): CommittedDesign[D] =
      CommittedDesign(staged.newStage(Printer.commit(stagedDB, co.commitPath(stagedDB))))
    def printGenFiles(using PrinterOptions): CompiledDesign[D] =
      Printer.printGenFiles(staged.stagedDB)
      cd
    def addFiles(files: (Iterable[String] | String)*): CompiledDesign[D] =
      import StagedDesign.addFiles as addFiles2
      staged.addFiles2(files*)
  end extension
end CompiledDesign
