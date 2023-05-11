package dfhdl.compiler.stages

import dfhdl.compiler.printing.Printer
import dfhdl.core.Design

opaque type CompiledDesign[D <: Design] = StagedDesign[D]
object CompiledDesign:
  def apply[D <: Design](sd: StagedDesign[D]): CompiledDesign[D] = sd
  extension [D <: Design](cd: CompiledDesign[D])
    def staged: StagedDesign[D] = cd
    def toFolder(path: String = cd.stagedDB.top.dclName): CommittedDesign[D] =
      CommittedDesign(staged.newStage(Printer.toFolder(staged.stagedDB, path)))
    def printGenFiles: CompiledDesign[D] =
      Printer.printGenFiles(staged.stagedDB)
      cd
