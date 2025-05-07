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
import DFVal.Func.Op as FuncOp

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
          case dfVal: DFVal if dfVal.dealias.get.asInstanceOf[DFVal.Dcl].isReg => ".din"
          case _                                                               => ""
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
  def csGoto(goto: Goto): String
  def csDFRange(range: DFRange): String
  def csWait(wait: Wait): String
  def csTextOut(textOut: TextOut): String
  // def csTimer(timer: Timer): String
  def csClkEdgeCfg(edge: ClkCfg.Edge): String =
    edge match
      case ClkCfg.Edge.Rising  => "ClkCfg.Edge.Rising"
      case ClkCfg.Edge.Falling => "ClkCfg.Edge.Falling"
  def csClkCfg(clkCfg: ClkCfg): String =
    clkCfg match
      case _: None.type => "None"
      case ClkCfg.Explicit(edge, rate, portName, _) =>
        val csRate = rate._2 match
          case _: DFTime.Unit => csDFTimeData(rate.asInstanceOf[(BigDecimal, DFTime.Unit)])
          case _: DFFreq.Unit => csDFFreqData(rate.asInstanceOf[(BigDecimal, DFFreq.Unit)])
        s"ClkCfg(${csClkEdgeCfg(edge)}, $csRate, $portName)"
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
      case RstCfg.Explicit(mode, active, portName, _) =>
        s"RstCfg(${csRstModeCfg(mode)}, ${csRstActiveCfg(active)}, $portName)"
  def csRTDomainCfg(clkCfg: ClkCfg, rstCfg: RstCfg): String =
    s"""RTDomainCfg(
       |    clkCfg = ${printer.csClkCfg(clkCfg)},
       |    rstCfg = ${printer.csRstCfg(rstCfg)}
       |)""".stripMargin
  def csRTDomainCfg(cfg: RTDomainCfg): String =
    cfg match
      case RTDomainCfg.Derived => "Derived"
      case RTDomainCfg.Explicit(name, clkCfg, rstCfg) =>
        if (name.isEmpty) csRTDomainCfg(clkCfg, rstCfg)
        else name
      case RTDomainCfg.Related(_) => ??? // should not be printed
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
      case pb: ProcessBlock                => csProcessBlock(pb)
      case stepBlock: StepBlock            => csStepBlock(stepBlock)
      case forBlock: DFLoop.DFForBlock     => csDFForBlock(forBlock)
      case whileBlock: DFLoop.DFWhileBlock => csDFWhileBlock(whileBlock)
      case domain: DomainBlock             => csDomainBlock(domain)
      // case timer: Timer        => csTimer(timer)
      case goto: Goto       => csGoto(goto)
      case wait: Wait       => csWait(wait)
      case textOut: TextOut => csTextOut(textOut)
      case _                => ???
    s"${printer.csDocString(member.meta)}${printer.csAnnotations(member.meta)}$cs"
  end csDFMember
  def designFileName(designName: String): String
  def globalFileName: String
  def csGlobalFileContent: String =
    csGlobalConstIntDcls + csGlobalTypeDcls + csGlobalConstNonIntDcls
  val alignEnable = printerOptions.align
  def alignCode(cs: String): String
  val colorEnable = printerOptions.color
  def colorCode(cs: String): String
  import io.AnsiColor._
  val keywordColor: String = s"$BLUE$BOLD"
  val keyword2Color: String = s"$MAGENTA$BOLD"
  val typeColor: String = "\u001B[38;5;94m"
  val commentColor: String = GREEN
  final def formatCode(cs: String): String =
    val alignedContents = if (alignEnable) alignCode(cs) else cs
    if (colorEnable) colorCode(alignedContents) else alignedContents
  final def csFile(design: DFDesignBlock): String =
    val designDcl = design.instMode match
      case InstMode.Def => csDFDesignDefDcl(design)
      case _            => csDFDesignBlockDcl(design)
    s"${csDocString(design.dclMeta)}$designDcl"
  def dfhdlDefsFileName: String
  def dfhdlSourceContents: String
  final def printedDB: DB =
    val designDB = getSet.designDB
    val dfhdlSourceFile: Option[SourceFile] =
      if (dfhdlDefsFileName.nonEmpty)
        Some(
          SourceFile(
            SourceOrigin.Compiled,
            SourceType.DFHDLDef,
            dfhdlDefsFileName,
            dfhdlSourceContents
          )
        )
      else None
    val globalSourceFile =
      SourceFile(
        SourceOrigin.Compiled,
        SourceType.GlobalDef,
        globalFileName,
        formatCode(csGlobalFileContent)
      )
    val compiledFiles = Iterable(
      dfhdlSourceFile,
      Some(globalSourceFile),
      designDB.uniqueDesignMemberList.view.map { case (block: DFDesignBlock, _) =>
        val sourceType = block.instMode match
          case _: DFDesignBlock.InstMode.BlackBox => SourceType.BlackBox
          case _                                  => SourceType.Design
        SourceFile(
          SourceOrigin.Compiled,
          sourceType,
          designFileName(block.dclName),
          formatCode(csFile(block))
        )
      }
    ).flatten
    // removing existing compiled/committed files and adding the newly compiled files
    val srcFiles = designDB.srcFiles.filter {
      case SourceFile(sourceOrigin = SourceOrigin.Compiled | SourceOrigin.Committed) => false
      case _                                                                         => true
    } ++ compiledFiles
    designDB.copy(srcFiles = srcFiles)
  end printedDB

  final def csDB: String =
    val designDB = getSet.designDB
    val csFileList = designDB.uniqueDesignMemberList.collect {
      case (block: DFDesignBlock, _) if printerOptions.designPrintFilter(block) =>
        formatCode(csFile(block))
    }
    s"${formatCode(
        csGlobalConstIntDcls + csGlobalTypeDcls + csGlobalConstNonIntDcls
      ).emptyOr(v => s"$v\n")}${csFileList.mkString("\n")}\n"
  end csDB
end Printer

object Printer:
  def printBackendCode(db: DB)(using po: PrinterOptions): Unit =
    val srcTypeFilter: SourceType => Boolean =
      if (po.showGlobals)
        srcType => srcType == SourceType.Design || srcType == SourceType.GlobalDef
      else srcType => srcType == SourceType.Design
    val srcFiles = db.srcFiles.view.filter(srcFile => srcTypeFilter(srcFile.sourceType))
    srcFiles.foreach {
      case srcFile @ SourceFile(
            SourceOrigin.Compiled | SourceOrigin.Committed,
            _,
            path,
            contents
          ) =>
        println("=======================================")
        println(srcFile.sourceOrigin)
        println(path)
        println("=======================================")
        println(contents)
        println("")
      case _ =>
    }
  end printBackendCode
  def commit(db: DB, topCommitPathStr: String): DB =
    val folderPath = Paths.get(topCommitPathStr).resolve("hdl")
    if (!Files.exists(folderPath))
      Files.createDirectories(folderPath)
    val updatedSrcFiles = db.srcFiles.map {
      case srcFile @ SourceFile(SourceOrigin.Compiled, _, filePathStr, contents) =>
        val commitPathAbs =
          if (Paths.get(filePathStr).isAbsolute) filePathStr
          else folderPath.resolve(filePathStr).toAbsolutePath.normalize().toString
        val commitPathSaved =
          if (Paths.get(filePathStr).isAbsolute) filePathStr
          else Paths.get("hdl").resolve(filePathStr).toString()
        val pw = new FileWriter(commitPathAbs)
        pw.write(contents.decolor)
        pw.close()
        srcFile.copy(sourceOrigin = SourceOrigin.Committed, path = commitPathSaved)
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
  def csGoto(goto: Goto): String = goto.stepRef.get match
    case stepBlock: StepBlock => stepBlock.getName
    case Goto.ThisStep        => "ThisStep"
    case Goto.NextStep        => "NextStep"
    case Goto.FirstStep       => "FirstStep"
  def csDFRange(range: DFRange): String =
    val op = range.op match
      case DFRange.Op.To    => "to"
      case DFRange.Op.Until => "until"
    val csBy = range.stepRef.refCodeString match
      case "1" => ""
      case cs  => s" by $cs"
    s"${range.startRef.refCodeString} ${op} ${range.endRef.refCodeString}$csBy"
  def csWait(wait: Wait): String =
    val trigger = wait.triggerRef.get
    trigger.dfType match
      case _: DFBoolOrBit =>
        trigger match
          case DFVal.Func(op = FuncOp.rising | FuncOp.falling) =>
            s"waitUntil(${wait.triggerRef.refCodeString})"
          case DFVal.Func(op = FuncOp.unary_!, args = List(triggerRef)) =>
            s"waitUntil(${triggerRef.refCodeString})"
          case _ =>
            s"waitWhile(${wait.triggerRef.refCodeString})"
      case DFTime => s"${wait.triggerRef.refCodeString}.wait"
      case _ =>
        wait.triggerRef.get.getConstData match
          // simplify display for int constant waits
          case Some(Some(value: BigInt)) if value.isValidInt =>
            s"${value}.cy.wait"
          case _ =>
            s"${wait.triggerRef.refCodeString}.cy.wait"
    end match
  end csWait
  def csTextOut(textOut: TextOut): String =
    val msg =
      textOut.op match
        case TextOut.Op.Debug =>
          textOut.msgArgs.view.map(_.refCodeString).mkString(", ")
        case _ =>
          textOut.msgParts.view.map(scalaToDFHDLString).coalesce(
            textOut.msgArgs.view.map(a => s"$${${a.refCodeString}}")
          ).mkString.emptyOr(m => s"s\"$m\"")
      end match
    textOut.op match
      case TextOut.Op.Finish => "finish()"
      case TextOut.Op.Report(severity) =>
        val csSeverity = if (severity == TextOut.Severity.Info) "" else s", Severity.${severity}"
        s"report($msg$csSeverity)"
      case TextOut.Op.Assert(assertionRef, severity) =>
        val csSeverity = if (severity == TextOut.Severity.Error) "" else s", Severity.${severity}"
        s"assert(${assertionRef.refCodeString}${msg.emptyOr(m => s", $m")}$csSeverity)"
      case TextOut.Op.Print   => s"print($msg)"
      case TextOut.Op.Println => s"println($msg)"
      case TextOut.Op.Debug   => s"debug($msg)"
    end match
  end csTextOut
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
  // def csTimer(timer: Timer): String =
  //   val timerBody = timer match
  //     case p: Timer.Periodic =>
  //       (p.triggerRef.get, p.rateOpt) match
  //         case (DFMember.Empty, None)       => "Timer()"
  //         case (DFMember.Empty, Some(rate)) => s"Timer(${csRateUnit(rate)})"
  //         case (trigger: DFVal, None) =>
  //           s"Timer(${p.triggerRef.refCodeString})"
  //         case (trigger: DFVal, Some(rate)) =>
  //           s"Timer(${p.triggerRef.refCodeString},${csRateUnit(rate)})"
  //         case _ => ??? // impossible
  //     case f: Timer.Func =>
  //       val argStr = f.arg match
  //         case r: Ratio => csRatioUnit(r)
  //         case t: Time  => csTimeUnit(t)
  //       s"${f.sourceRef.refCodeString} ${f.op} $argStr"
  //   if (timer.isAnonymous) timerBody else s"val ${timer.getName} = $timerBody"
  // end csTimer
  def globalFileName: String = s"${getSet.designDB.top.dclName}_globals.scala"
  def designFileName(designName: String): String = s"$designName.scala"
  def dfhdlDefsFileName: String = "" // no need in DFHDL code generation
  def dfhdlSourceContents: String = "" // no need in DFHDL code generation
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
      "case", "final", "for", "while", "until", "to", "by")
  val dfhdlKW: Set[String] =
    Set("VAR", "REG", "din", "IN", "OUT", "INOUT", "VAL", "DFRET", "CONST", "DFDesign", "RTDesign",
      "EDDesign", "DFDomain", "RTDomain", "EDDomain", "process", "forever", "all", "init", "step",
      "goto", "wait", "assert", "report", "print", "println", "debug", "finish")
  val dfhdlOps: Set[String] = Set("<>", ":=", ":==")
  val dfhdlTypes: Set[String] =
    Set("Bit", "Boolean", "Int", "UInt", "SInt", "Bits", "X", "Encoded", "Struct", "Opaque",
      "StartAt", "OneHot", "Grey", "Unit", "Time", "Freq", "String", "fs", "ns", "ps", "us", "ms",
      "sec", "min", "hr", "Hz", "KHz", "MHz", "GHz")
  def colorCode(cs: String): String =
    cs
      .colorWords(scalaKW, keywordColor)
      .colorWords(dfhdlKW, keyword2Color)
      .colorOps(dfhdlOps, keyword2Color)
      .colorWords(dfhdlTypes, typeColor)
      .colorLineComment("//", commentColor)
      .colorBlockComment("/\\*", "\\*/", commentColor)
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
