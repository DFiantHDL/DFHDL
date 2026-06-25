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
import java.io.File.separatorChar

protected trait AbstractPrinter:
  type TPrinter <: Printer
  given printer: TPrinter
  given getSet: MemberGetSet
  given printerOptions: PrinterOptions
  val tupleSupportEnable: Boolean
  // Construct a printer of the same concrete type bound to `subGetSet`. Used to
  // render each sub-DB's design (and its globals) under that sub-DB's own getSet
  // when the DB is a hierarchical root — the root's own getSet throws on ref
  // resolution, so the trait-level print methods dispatch through one of these
  // per sub-DB.
  protected def withGetSet(subGetSet: MemberGetSet): TPrinter
  // The printer to use when rendering `design`'s own members: for a hierarchical
  // root, a sub-printer bound to that design's sub-DB getSet (its members live
  // there); for a flat DB, `this` printer (every design shares one getSet). Used
  // for cross-design renders such as a blackbox component/module declaration,
  // where a design's body references another design's port list.
  protected final def printerForDesign(design: DFDesignBlock): TPrinter =
    getSet.designDB.rootDB.subDBs.get(design.ownerRef) match
      case Some(sub) => withGetSet(sub.getSet)
      case None      => printer
end AbstractPrinter

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
          if (net.isViaConnection) rhsOrig match
            case pbns: DFVal.PortByNameSelect => normalizeViaConnection
            case _                            =>
              normalizeViaConnection && rhsOrig.getOwner.isSameOwnerDesignAs(net)
          // swapped if the net is a regular connection and the RHS is receiver and
          // as long as the LHS is not OPEN
          else swapped && normalizeConnection && !lhsVal.isInstanceOf[DFVal.Special]
        val directionStr =
          lhsOrig match
            case dfVal: DFVal =>
              if (dfVal.getConnectionsTo.contains(net) ^ swapLR) "<--"
              else "-->"
        val (lhsRef, rhsRef) = if (swapLR) (net.rhsRef, net.lhsRef) else (net.lhsRef, net.rhsRef)
        def csNode(ref: DFNet.Ref): String =
          ref.get match
            case pbns: DFVal.PortByNameSelect if net.isViaConnection => pbns.portName
            case _                                                   => ref.refCodeString
        val lhsStr = csNode(lhsRef)
        val rhsStr = csNode(rhsRef)
        net.op.runtimeChecked match
          case DFNet.Op.Connection     => csConnection(lhsStr, rhsStr, directionStr)
          case DFNet.Op.ViaConnection  => csViaConnection(lhsStr, rhsStr, directionStr)
          case DFNet.Op.LazyConnection => csLazyConnection(lhsStr, rhsStr, directionStr)
        end match
      case _ =>
        val lhsDin = net.lhsRef.get match
          case dfVal: DFVal if dfVal.dealias.get.asInstanceOf[DFVal.Dcl].isReg => ".din"
          case _                                                               => ""
        val lhsShared =
          net.lhsRef.get.dealias.get.asInstanceOf[DFVal.Dcl].modifier.isShared
        val lhsStr = net.lhsRef.refCodeString + lhsDin
        val rhsStr = net.rhsRef.refCodeString
        net.op.runtimeChecked match
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
  def csCommentInline(comment: String): String
  def csCommentEOL(comment: String): String
  def csDocString(doc: String): String
  final def csDocString(meta: Meta): String =
    meta.docOpt.map(printer.csDocString).map(x => s"$x\n").getOrElse("")
  def csAnnotations(annotations: List[annotation.HWAnnotation]): String
  final def csDFMember(member: DFMember): String =
    val cs = member match
      case dfVal: DFVal.CanBeExpr if dfVal.isAnonymous => csDFValExpr(dfVal)
      case dfVal: DFVal                                => csDFValNamed(dfVal)
      case net: DFNet                                  => csDFNet(net)
      case inst: DFDesignInst                          =>
        inst.getDesignBlock.instMode match
          case InstMode.Def => csDFDesignDefInst(inst)
          case _            => csDFDesignBlockInst(inst)
      case pb: ProcessBlock                => csProcessBlock(pb)
      case fb: ForkBlock                   => csForkBlock(fb)
      case lb: LocalBlock                  => csLocalBlock(lb)
      case stepBlock: StepBlock            => csStepBlock(stepBlock)
      case forBlock: DFLoop.DFForBlock     => csDFForBlock(forBlock)
      case whileBlock: DFLoop.DFWhileBlock => csDFWhileBlock(whileBlock)
      case domain: DomainBlock             => csDomainBlock(domain)
      // case timer: Timer        => csTimer(timer)
      case goto: Goto       => csGoto(goto)
      case wait: Wait       => csWait(wait)
      case textOut: TextOut => csTextOut(textOut)
      // DFDesignBlock no longer renders an instantiation inside an owner
      // body — that now flows through DFDesignInst. The declaration file is
      // still produced by `csFile` via `csDFDesignBlockDcl`.
      case _: DFDesignBlock => ""
      case _                => ???
    s"${printer.csDocString(member.meta)}${printer.csAnnotations(member.meta.annotations)}$cs"
  end csDFMember
  def designFileName(designName: String): String
  def globalFileName: String
  protected def hasGlobalContentCheck: Boolean =
    val designDB = getSet.designDB
    val anyNamedGlobal =
      if (designDB.isRoot)
        designDB.subDBs.view.values.exists(_.membersGlobals.exists(!_.isAnonymous))
      else designDB.membersGlobals.exists(!_.isAnonymous)
    anyNamedGlobal || csGlobalTypeDcls.nonEmpty
  lazy val hasGlobalContent: Boolean = hasGlobalContentCheck
  def csGlobalFileContent: String =
    sn"""|$csGlobalConstIntDcls
         |$csGlobalTypeDcls
         |$csGlobalConstNonIntDcls"""
  def alignCode(cs: String): String
  def colorCode(cs: String): String
  import io.AnsiColor._
  val keywordColor: String = s"$BLUE$BOLD"
  val keyword2Color: String = s"$MAGENTA$BOLD"
  val typeColor: String = "\u001B[38;5;94m"
  val commentColor: String = GREEN
  final def formatCode(cs: String, withColor: Boolean = printerOptions.color): String =
    val alignedContents = if (printerOptions.align) alignCode(cs) else cs
    if (withColor) colorCode(alignedContents) else alignedContents
  private var currentDesign: Option[DFDesignBlock] = None
  def getCurrentDesign: DFDesignBlock = currentDesign.get
  final def csFile(design: DFDesignBlock): String =
    currentDesign = Some(design)
    val designDcl = design.instMode match
      case InstMode.Def => csDFDesignDefDcl(design)
      case _            => csDFDesignBlockDcl(design)
    s"${csDocString(design.dclMeta)}$designDcl"
  def dfhdlDefsFileName: String
  def dfhdlSourceContents: String
  val hdlFolderName: String = "hdl"
  final def printedDB: DB =
    val designDB = getSet.designDB
    val dfhdlSourceFile: Option[SourceFile] =
      if (dfhdlDefsFileName.nonEmpty)
        Some(
          SourceFile(
            SourceOrigin.Compiled,
            SourceType.DFHDLDef,
            hdlFolderName + separatorChar + dfhdlDefsFileName,
            dfhdlSourceContents
          )
        )
      else None
    val globalSourceFile: Option[SourceFile] =
      if (hasGlobalContent)
        Some(
          SourceFile(
            SourceOrigin.Compiled,
            SourceType.GlobalDef,
            hdlFolderName + separatorChar + globalFileName,
            formatCode(csGlobalFileContent, withColor = false)
          )
        )
      else None
    val compiledFiles = Iterable(
      dfhdlSourceFile,
      globalSourceFile,
      designPrinters.view
        // A foreign IP supplies its own HDL wrapper as a bundled resource (copied into the project
        // at commit), so DFHDL must not generate an HDL file for it (that would duplicate the
        // wrapper module/entity).
        .filterNot { case (block, _) => block.isForeignIPBlackbox }
        .map { case (block, p) =>
          val sourceType = block.instMode match
            case _: DFDesignBlock.InstMode.BlackBox => SourceType.BlackBox
            case _                                  => SourceType.Design
          SourceFile(
            SourceOrigin.Compiled,
            sourceType,
            hdlFolderName + separatorChar + designFileName(block.dclName),
            formatCode(p.csFile(block), withColor = false)
          )
        }
    ).flatten
    // removing existing compiled/committed files and adding the newly compiled files
    val srcFiles = designDB.srcFiles.filter {
      case SourceFile(sourceOrigin = SourceOrigin.Compiled | SourceOrigin.Committed) => false
      case _                                                                         => true
    } ++ compiledFiles
    designDB.update(srcFiles = srcFiles)
  end printedDB

  val printVendorIPBlackbox: Boolean = false

  // The (design block, printer-bound-to-its-getSet) pairs to render, in order.
  // Flat DB: every design under `this` printer. Hierarchical root: each sub-DB's
  // design under a sub-printer bound to that sub-DB's getSet (the root's own
  // getSet throws on ref resolution).
  protected final def designPrinters: List[(DFDesignBlock, TPrinter)] =
    val designDB = getSet.designDB
    if (designDB.isRoot)
      // Flat `designMemberList` prints designs in post-order DFS of the design
      // tree (children in instantiation order, then the parent); the `subDBs`
      // ListMap is pre-order (parent first). Reorder to post-order so the
      // hierarchical output matches the flat output design-for-design.
      val childrenOf = mutable.LinkedHashMap.empty[DFOwner.Ref, mutable.ListBuffer[DB]]
      designDB.subDBs.values.foreach { sub =>
        sub.parentSubDBOpt.foreach { parent =>
          childrenOf.getOrElseUpdate(parent.top.ownerRef, mutable.ListBuffer.empty) += sub
        }
      }
      def postOrder(sub: DB): List[DB] =
        childrenOf.getOrElse(sub.top.ownerRef, mutable.ListBuffer.empty).toList
          .flatMap(postOrder) :+ sub
      postOrder(designDB.topDB).map(sub => sub.top -> withGetSet(sub.getSet))
    else
      designDB.designMemberList.collect { case (block: DFDesignBlock, _) => block -> printer }
    end if
  end designPrinters

  final def csDB: String =
    val csFileList = designPrinters.collect {
      case (block, p)
          if printerOptions.designPrintFilter(block) &&
            // external IP blackboxes (vendor IP, foreign IP) are not rendered as modules by the
            // HDL backends — vendor IP is generated by the vendor tool, foreign IP ships its HDL
            // wrapper as a resource. The DFHDL printer (printVendorIPBlackbox) still renders them.
            (!block.isExternalIPBlackbox || printVendorIPBlackbox) =>
        formatCode(p.csFile(block))
    }
    val globals = formatCode(
      sn"""|$csGlobalConstIntDcls
           |$csGlobalTypeDcls
           |$csGlobalConstNonIntDcls"""
    )
    sn"""|$globals
         |
         |${csFileList.mkString("\n")}
         |""".stripMargin
  end csDB
end Printer

object Printer:
  def printBackendCode(printer: Printer)(using po: PrinterOptions): Unit =
    val db = printer.getSet.designDB
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
        if (po.color) println(printer.colorCode(contents))
        else println(contents)
        println("")
      case _ =>
    }
  end printBackendCode
  def commit(db: DB, topCommitPathStr: String): DB =
    val folderPath = Paths.get(topCommitPathStr)
    if (!Files.exists(folderPath))
      Files.createDirectories(folderPath)
    val updatedSrcFiles = db.srcFiles.map {
      case srcFile @ SourceFile(SourceOrigin.Compiled, _, filePathStr, contents) =>
        val commitPathAbs =
          if (Paths.get(filePathStr).isAbsolute) filePathStr
          else folderPath.resolve(filePathStr).toAbsolutePath.normalize().toString
        val commitPathFolder = Paths.get(commitPathAbs).getParent
        if (!Files.exists(commitPathFolder))
          Files.createDirectories(commitPathFolder)
        val pw = new FileWriter(commitPathAbs)
        pw.write(contents)
        pw.close()
        srcFile.copy(sourceOrigin = SourceOrigin.Committed)
      case other => other
    }
    db.update(srcFiles = updatedSrcFiles)
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
  protected def withGetSet(subGetSet: MemberGetSet): DFPrinter =
    new DFPrinter(using subGetSet, printerOptions)
  override val printVendorIPBlackbox: Boolean = true
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
        // `ir.Wait(X)` resumes when X is true: `waitUntil(X)`. A negated trigger `not inner`
        // renders back as `waitWhile(inner)`.
        trigger match
          case DFVal.Func(op = FuncOp.rising | FuncOp.falling) =>
            s"waitUntil(${wait.triggerRef.refCodeString})"
          case DFVal.Func(op = FuncOp.unary_!, args = List(innerRef)) =>
            s"waitWhile(${innerRef.refCodeString})"
          case _ =>
            s"waitUntil(${wait.triggerRef.refCodeString})"
      case DFTime => s"${wait.triggerRef.refCodeString}.wait"
      case _      =>
        wait.triggerRef.get.getConstData[Option[BigInt]] match
          // simplify display for int constant waits
          case ConstData.KnownConst(Some(value: BigInt)) if value.isValidInt =>
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
      case TextOut.Op.Finish           => "finish()"
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
  def csAnnotations(annotations: List[annotation.HWAnnotation]): String =
    if (annotations.isEmpty) ""
    else annotations.view.map(_.codeString).mkString("", "\n", "\n")
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
  def globalFileName: String = s"${getSet.topName}_globals.scala"
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
  val scalaKW: Set[String] = Set(
    "class", "def", "end", "enum", "extends", "new", "object", "val", "if", "else", "match",
    "case", "final", "for", "while", "until", "to", "by"
  )
  val dfhdlKW: Set[String] = Set(
    "VAR", "REG", "din", "IN", "OUT", "INOUT", "VAL", "DFRET", "CONST", "DFDesign", "RTDesign",
    "EDDesign", "DFDomain", "RTDomain", "EDDomain", "process", "forever", "all", "init", "step",
    "goto", "wait", "assert", "report", "print", "println", "debug", "finish", "CLK_FREQ"
  )
  val dfhdlOps: Set[String] = Set("<>", ":=", ":==")
  val dfhdlTypes: Set[String] = Set(
    "Bit", "Boolean", "Int", "UInt", "SInt", "Bits", "X", "Encoded", "Struct", "Opaque",
    "StartAt", "OneHot", "Gray", "Unit", "Time", "Freq", "String", "Double", "fs", "ns", "ps", "us",
    "ms", "sec", "min", "hr", "Hz", "KHz", "MHz", "GHz"
  )
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
