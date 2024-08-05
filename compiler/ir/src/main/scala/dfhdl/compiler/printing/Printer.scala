package dfhdl.compiler
package printing
import ir.*
import dfhdl.internals.*
import scala.collection.mutable
import analysis.*
import java.io.FileWriter
import java.nio.file.{Paths, Files}
import dfhdl.options.PrinterOptions
import DFDesignBlock.InstMode

protected trait AbstractPrinter:
  type TPrinter <: Printer
  given printer: TPrinter
  given getSet: MemberGetSet
  given printerOptions: PrinterOptions
  val tupleSupportEnable: Boolean

trait Printer
    extends AbstractTypePrinter,
      AbstractDataPrinter,
      AbstractValPrinter,
      AbstractOwnerPrinter:
  def csViaConnectionSep: String
  val normalizeViaConnection: Boolean
  val normalizeConnection: Boolean
  def csAssignment(lhsStr: String, rhsStr: String, shared: Boolean): String
  def csNBAssignment(lhsStr: String, rhsStr: String): String
  def csConnection(lhsStr: String, rhsStr: String, directionStr: String): String
  def csViaConnection(lhsStr: String, rhsStr: String, directionStr: String): String
  def csLazyConnection(lhsStr: String, rhsStr: String, directionStr: String): String
  final def csDFNet(net: DFNet): String =
    net match
      case DFNet.Connection(lhsVal, rhsVal, swapped) =>
        val (lhsOrig, rhsOrig) = if (swapped) (rhsVal, lhsVal) else (lhsVal, rhsVal)
        // True if the net needs to be shown in a swapped order.
        // Normalized via connections always have the internal port on the LHS.
        // Normalized connections always have the receiver port on the LHS.
        val swapLR =
          // swapped if the net is a via and the RHS is the internal port
          if (net.isViaConnection)
            normalizeViaConnection && rhsOrig.getOwner.isSameOwnerDesignAs(net)
          // swapped if the net is a regular connection and the RHS is receiver
          else swapped && normalizeConnection
        val directionStr =
          lhsOrig match
            case dfIfc: DFInterfaceOwner => "<->"
            case dfVal: DFVal =>
              if (dfVal.getConnectionTo.contains(net) ^ swapLR) "<--"
              else "-->"
        val (lhsRef, rhsRef) = if (swapLR) (net.rhsRef, net.lhsRef) else (net.lhsRef, net.rhsRef)
        val lhsStr = if (lhsRef.isViaRef) lhsRef.get.stripPortSel.getName else lhsRef.refCodeString
        val rhsStr = if (rhsRef.isViaRef) rhsRef.get.stripPortSel.getName else rhsRef.refCodeString
        (net.op: @unchecked) match
          case DFNet.Op.Connection     => csConnection(lhsStr, rhsStr, directionStr)
          case DFNet.Op.ViaConnection  => csViaConnection(lhsStr, rhsStr, directionStr)
          case DFNet.Op.LazyConnection => csLazyConnection(lhsStr, rhsStr, directionStr)
        end match
      case _ =>
        val lhsDin = net.lhsRef.get match
          case dfVal: DFVal if dfVal.dealias.get.asInstanceOf[DFVal.Dcl].modifier.isReg => ".din"
          case _                                                                        => ""
        val lhsShared = net.lhsRef.get match
          case dfVal: DFVal => dfVal.dealias.get.asInstanceOf[DFVal.Dcl].modifier.isShared
          case _            => false
        val lhsStr = net.lhsRef.refCodeString + lhsDin
        val rhsStr = net.rhsRef.refCodeString
        (net.op: @unchecked) match
          case DFNet.Op.Assignment   => csAssignment(lhsStr, rhsStr, lhsShared)
          case DFNet.Op.NBAssignment => csNBAssignment(lhsStr, rhsStr)
        end match
  end csDFNet
  def csOpenKeyWord: String
  def csTimeUnit(time: Time): String = time.toString()
  def csFreqUnit(freq: Freq): String = freq.toString()
  def csRateUnit(rate: Rate): String = rate.toString()
  def csRatioUnit(ratio: Ratio): String = s"${ratio.value}"
  def csTimer(timer: Timer): String
  def csClkEdgeCfg(edge: ClkCfg.Edge): String =
    edge match
      case ClkCfg.Edge.Rising  => "ClkCfg.Edge.Rising"
      case ClkCfg.Edge.Falling => "ClkCfg.Edge.Falling"
  def csClkCfg(clkCfg: ClkCfg): String =
    clkCfg match
      case _: None.type => "None"
      case ClkCfg.Explicit(edge, rate) =>
        s"ClkCfg(${csClkEdgeCfg(edge)}, ${csRateUnit(rate)})"
  def csRstModeCfg(mode: RstCfg.Mode): String =
    mode match
      case RstCfg.Mode.Sync  => "RstCfg.Mode.Sync"
      case RstCfg.Mode.Async => "RstCfg.Mode.Async"
  def csRstActiveCfg(active: RstCfg.Active): String =
    active match
      case RstCfg.Active.High => "RstCfg.Active.High"
      case RstCfg.Active.Low  => "RstCfg.Active.Low"
  def csRstCfg(rstCfg: RstCfg): String =
    rstCfg match
      case _: None.type => "None"
      case RstCfg.Explicit(mode, active) =>
        s"RstCfg(${csRstModeCfg(mode)}, ${csRstActiveCfg(active)})"
  def csRTDomainCfg(clkCfg: ClkCfg, rstCfg: RstCfg): String =
    s"""RTDomainCfg(
       |    clkCfg = ${printer.csClkCfg(clkCfg)},
       |    rstCfg = ${printer.csRstCfg(rstCfg)}
       |)""".stripMargin
  def csRTDomainCfg(cfg: RTDomainCfg): String =
    cfg match
      case RTDomainCfg.DerivedCfg => "DerivedCfg"
      case RTDomainCfg.Explicit(name, clkCfg, rstCfg) =>
        if (name.isEmpty) csRTDomainCfg(clkCfg, rstCfg)
        else name
      case RTDomainCfg.RelatedCfg(_) => ??? // should not be printed
  def csCommentInline(comment: String): String
  def csCommentEOL(comment: String): String
  def csDocString(doc: String): String
  final def csDocString(meta: Meta): String =
    meta.docOpt.map(printer.csDocString).map(x => s"$x\n").getOrElse("")
  def csAnnotations(meta: Meta): String
  final def csDFMember(member: DFMember): String =
    val cs = member match
      case dfVal: DFVal.CanBeExpr if dfVal.isAnonymous => csDFValExpr(dfVal)
      case dfVal: DFVal                                => csDFValNamed(dfVal)
      case net: DFNet                                  => csDFNet(net)
      case design: DFDesignBlock =>
        design.instMode match
          case InstMode.Def => csDFDesignDefInst(design)
          case _            => csDFDesignBlockInst(design)
      case pb: ProcessBlock    => csProcessBlock(pb)
      case domain: DomainBlock => csDomainBlock(domain)
      case timer: Timer        => csTimer(timer)
      case _                   => ???
    s"${printer.csDocString(member.meta)}${printer.csAnnotations(member.meta)}$cs"
  def designFileName(designName: String): String
  def globalFileName: String
  def csGlobalFileContent: String =
    csGlobalTypeDcls + csGlobalConstDcls
  val alignEnable = printerOptions.align
  def alignCode(cs: String): String
  val colorEnable = printerOptions.color
  def colorCode(cs: String): String
  import io.AnsiColor._
  val keywordColor: String = s"$BLUE$BOLD"
  val keyword2Color: String = s"$MAGENTA$BOLD"
  val typeColor: String = "\u001B[38;5;94m"
  final def formatCode(cs: String): String =
    val alignedContents = if (alignEnable) alignCode(cs) else cs
    if (colorEnable) colorCode(alignedContents) else alignedContents
  final def csFile(design: DFDesignBlock): String =
    val designDcl = design.instMode match
      case InstMode.Def => csDFDesignDefDcl(design)
      case _            => csDFDesignBlockDcl(design)
    s"${csDocString(design.dclMeta)}$designDcl"
  final def printedDB: DB =
    val designDB = getSet.designDB
    val globalSourceFile =
      SourceFile(
        SourceOrigin.Compiled,
        SourceType.Design.GlobalDef,
        globalFileName,
        formatCode(csGlobalFileContent)
      )
    val compiledFiles = globalSourceFile :: designDB.uniqueDesignMemberList.map {
      case (block: DFDesignBlock, _) =>
        val sourceType = block.instMode match
          case _: DFDesignBlock.InstMode.BlackBox => SourceType.Design.BlackBox
          case _                                  => SourceType.Design.Regular
        SourceFile(
          SourceOrigin.Compiled,
          sourceType,
          designFileName(block.dclName),
          formatCode(csFile(block))
        )
    }
    // removing existing compiled/committed files and adding the newly compiled files
    val srcFiles = designDB.srcFiles.filter {
      case SourceFile(SourceOrigin.Compiled | SourceOrigin.Committed, _, _, _) => false
      case _                                                                   => true
    } ++ compiledFiles
    designDB.copy(srcFiles = srcFiles)
  end printedDB

  final def csDB: String =
    val designDB = getSet.designDB
    val csFileList = designDB.uniqueDesignMemberList.collect {
      case (block: DFDesignBlock, _) if printerOptions.designPrintFilter(block) =>
        formatCode(csFile(block))
    }
    s"${formatCode(csGlobalTypeDcls + csGlobalConstDcls).emptyOr(v => s"$v\n")}${csFileList.mkString("\n")}\n"
  end csDB
end Printer

object Printer:
  def printGenFiles(db: DB)(using po: PrinterOptions): Unit =
    val srcFiles =
      if (po.showGlobals) db.srcFiles
      else db.srcFiles.drop(1)
    srcFiles.foreach {
      case srcFile @ SourceFile(
            SourceOrigin.Compiled | SourceOrigin.Committed,
            _,
            path,
            contents
          ) =>
        println("==========================================================")
        println(srcFile.sourceOrigin)
        println(path)
        println("==========================================================")
        println(contents)
        println("")
      case _ =>
    }
  end printGenFiles
  def commit(db: DB, folderPathStr: String): DB =
    val folderPath = Paths.get(folderPathStr)
    if (!Files.exists(folderPath))
      Files.createDirectories(folderPath)
    val updatedSrcFiles = db.srcFiles.map {
      case srcFile @ SourceFile(SourceOrigin.Compiled, _, filePathStr, contents) =>
        val commitPathStr =
          if (Paths.get(filePathStr).isAbsolute) filePathStr
          else folderPath.resolve(filePathStr).toAbsolutePath.normalize().toString
        val pw = new FileWriter(commitPathStr)
        pw.write(contents.decolor)
        pw.close()
        srcFile.copy(sourceOrigin = SourceOrigin.Committed, path = commitPathStr)
      case other => other
    }
    db.copy(srcFiles = updatedSrcFiles)
  end commit
end Printer

class DFPrinter(using val getSet: MemberGetSet, val printerOptions: PrinterOptions)
    extends Printer,
      DFTypePrinter,
      DFDataPrinter,
      DFValPrinter,
      DFOwnerPrinter:
  type TPrinter = DFPrinter
  given printer: TPrinter = this
  val tupleSupportEnable: Boolean = true
  def csViaConnectionSep: String = ""
  def csAssignment(lhsStr: String, rhsStr: String, shared: Boolean): String =
    s"$lhsStr := $rhsStr"
  def csNBAssignment(lhsStr: String, rhsStr: String): String =
    s"$lhsStr :== $rhsStr"
  def csConnection(lhsStr: String, rhsStr: String, directionStr: String): String =
    s"$lhsStr <> ${rhsStr.applyBrackets()}"
  def csViaConnection(lhsStr: String, rhsStr: String, directionStr: String): String =
    s"this.$lhsStr <>/*$directionStr*/ ${rhsStr.applyBrackets()}"
  def csLazyConnection(lhsStr: String, rhsStr: String, directionStr: String): String =
    s"$lhsStr `<LZ>`/*$directionStr*/ ${rhsStr.applyBrackets()}"
  val normalizeViaConnection: Boolean = true
  val normalizeConnection: Boolean = true
  def csOpenKeyWord: String = "OPEN"
  // to remove ambiguity in referencing a port inside a class instance we add `this.` as prefix
  def csCommentInline(comment: String): String =
    if (comment.contains('\n'))
      s"""/*
         |${comment.hindent}
         |*/""".stripMargin
    else s"/*$comment*/"
  def csCommentEOL(comment: String): String = s"// $comment"
  def csDocString(doc: String): String = doc.betterLinesIterator.mkString("/**", "\n  *", "*/")
  def csAnnotations(meta: Meta): String =
    if (meta.annotations.isEmpty) ""
    else meta.annotations.view.map(x => s"@hw.${x.codeString}").mkString("", "\n", "\n")
  def csTimer(timer: Timer): String =
    val timerBody = timer match
      case p: Timer.Periodic =>
        (p.triggerRef.get, p.rateOpt) match
          case (DFMember.Empty, None)       => "Timer()"
          case (DFMember.Empty, Some(rate)) => s"Timer(${csRateUnit(rate)})"
          case (trigger: DFVal, None) =>
            s"Timer(${p.triggerRef.refCodeString})"
          case (trigger: DFVal, Some(rate)) =>
            s"Timer(${p.triggerRef.refCodeString},${csRateUnit(rate)})"
          case _ => ??? // impossible
      case f: Timer.Func =>
        val argStr = f.arg match
          case r: Ratio => csRatioUnit(r)
          case t: Time  => csTimeUnit(t)
        s"${f.sourceRef.refCodeString} ${f.op} $argStr"
    if (timer.isAnonymous) timerBody else s"val ${timer.getName} = $timerBody"
  end csTimer
  def globalFileName: String = s"${getSet.designDB.top.dclName}_globals.scala"
  def designFileName(designName: String): String = s"$designName.scala"
  def alignCode(cs: String): String =
    cs
      .align("[ \\t]*val .*", "=", ".*<>.*")
      .align("[ \\t]*val .*", "<>", ".*")
      .align("[ \\t]*val .*<>.*", "init", ".*")
      .align("[ ]*[a-zA-Z0-9_.]+[ ]*", ":=|<>|:==", ".*")
      .align("[ ]*[a-zA-Z0-9_.]+[ ]*(?::=|<>|:==)", " ", ".*")
      // align enums
      .align("[ ]*case [a-zA-Z0-9_]+[ ]*", "extends", ".*")
      // align cases
      .align("[ ]*case [a-zA-Z0-9_.]+[ ]*", "=>", ".*")

  import io.AnsiColor._
  val scalaKW: Set[String] =
    Set("class", "def", "end", "enum", "extends", "new", "object", "val", "if", "else", "match",
      "case", "final")
  val dfhdlKW: Set[String] =
    Set("VAR", "REG", "din", "IN", "OUT", "INOUT", "VAL", "DFRET", "CONST", "DFDesign", "RTDesign",
      "EDDesign", "DFDomain", "RTDomain", "EDDomain", "process", "forever", "all", "init")
  val dfhdlOps: Set[String] = Set("<>", ":=", ":==")
  val dfhdlTypes: Set[String] =
    Set("Bit", "Boolean", "Int", "UInt", "SInt", "Bits", "X", "Encode", "Struct", "Opaque",
      "StartAt", "OneHot", "Grey")
  def colorCode(cs: String): String =
    cs
      .colorWords(scalaKW, keywordColor)
      .colorWords(dfhdlKW, keyword2Color)
      .colorOps(dfhdlOps, keyword2Color)
      .colorWords(dfhdlTypes, typeColor)
end DFPrinter

extension (member: DFMember)(using printer: Printer)
  def codeString: String =
    printer.csDFMember(member)
extension (dfType: DFType)(using printer: DFTypePrinter)
  def codeString: String =
    printer.csDFType(dfType)

def DefaultPrinter(using MemberGetSet): Printer =
  given PrinterOptions.Align = false
  new DFPrinter
