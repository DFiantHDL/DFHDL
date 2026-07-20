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

// A global declaration whose emission position is dependency-driven: a global constant or a
// global HDL method (see `AbstractPrinter.globalDeclsOrdered`). Global TYPE declarations are not
// part of this, as they always come first.
enum GlobalDecl derives CanEqual:
  case Const(member: DFMember)
  case Method(block: DFDesignBlock)

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
  // The code string of each PHANTOM member (port or parameter) of the method this
  // printer renders, keyed by the phantom's name INSIDE the def, and resolved at the def's
  // call site in the OWNING design's scope. A phantom materializes a value the def body
  // captured from its host, and the printed body must name that value as the host names it:
  // the phantom's own name is the captured path's LEAF (`sub.o` -> `o`), which denotes
  // nothing at the host's scope. Empty for every printer that is not rendering a def body.
  private var phantomActualsCS: Map[String, String] = Map.empty
  final def phantomActualOf(name: String): Option[String] = phantomActualsCS.get(name)
  protected def setPhantomActuals(actuals: Map[String, String]): Unit =
    phantomActualsCS = actuals
  // A printer for the method instantiated by `inst`, bound to the def's own getSet and
  // carrying the code strings of its phantom actuals as resolved HERE, at the call site, in
  // this printer's (the host design's) scope — so the rendered body names the captured values
  // as the host names them. `inst` must be a member of the design this printer renders.
  final private[printing] def methodPrinterAt(inst: DFDesignInst): TPrinter =
    val designDB = getSet.designDB
    val root = designDB.rootDB
    val defSubOpt = if (root.isRoot) root.subDBs.get(inst.designRef) else None
    val defPrinter = defSubOpt.map(sub => withGetSet(sub.getSet)).getOrElse(printer)
    val defGetSet = defSubOpt.map(_.getSet).getOrElse(getSet)
    val defMembers =
      defSubOpt.map(_.members).getOrElse(inst.getDesignBlock.members(MemberView.Folded))
    // A phantom input port is paired with its call-site connection BY ORDER: the harness
    // (`r__For_Plugin.designFromDef`) appends the phantom ports after the explicit ones and
    // connects them in that same order. Matching by name would not work — the PBNS records
    // the port's name as of the connection, while the def's port may be uniquified afterwards
    // (capturing `sub.o` into a def whose return port is also `o` yields `o_0`), and the
    // BODY prints the uniquified name.
    val phantomPorts = defMembers.collect {
      case dcl @ DclIn() if dcl.isPhantom => dcl
    }
    val phantomPBNS = designDB.designInstPBNS.getOrElse(inst, Nil).filter { pbns =>
      pbns.isIn && pbns.isPhantom
    }
    val portActuals =
      if (phantomPorts.length != phantomPBNS.length) Map.empty[String, String]
      else
        phantomPorts.lazyZip(phantomPBNS).map { (port, pbns) =>
          val DFNet.Connection(_, from: DFVal, _) = pbns.getConnectionsTo.head.runtimeChecked
          port.getName(using defGetSet) -> printer.csDFValRef(from, inst.getOwner)
        }.toMap
    // a phantom design parameter's actual is this call site's applied value. Parameters are
    // matched by name (the paramMap key IS the parameter's name, kept in sync by the harness).
    val phantomParams = defMembers.collect {
      case param: DFVal.DesignParam if param.isPhantom => param
    }
    val paramActuals = phantomParams.view.flatMap { param =>
      inst.paramMap.get(param.meta.name).map { ref =>
        param.getName(using defGetSet) -> printer.csDFValRef(ref.get, inst.getOwner)
      }
    }.toMap
    defPrinter.setPhantomActuals(portActuals ++ paramActuals)
    defPrinter
  end methodPrinterAt
  // A printer for the method called by `call`, bound to the def's own getSet and
  // carrying the code strings of its phantom actuals as resolved HERE, at the call site,
  // in this printer's (the host design's) scope. The call's args bind the formals
  // POSITIONALLY in formal member order, so a phantom formal's actual is simply the arg
  // at the phantom formal's index.
  final private[printing] def methodPrinterAt(call: DFVal.Func, designKey: StaticRef): TPrinter =
    val designDB = getSet.designDB
    val root = designDB.rootDB
    val defSubOpt = if (root.isRoot) root.subDBs.get(designKey) else None
    val defPrinter = defSubOpt.map(sub => withGetSet(sub.getSet)).getOrElse(printer)
    val defGetSet = defSubOpt.map(_.getSet).getOrElse(getSet)
    val defMembers =
      defSubOpt.map(_.members).getOrElse(designKey.getDesignBlock.members(MemberView.Folded))
    val formals = defMembers.collect {
      case dcl: DFVal.Dcl if dcl.isPortIn => dcl
    }
    val actuals = call.args.map(_.get)
    val phantomActuals =
      if (formals.length != actuals.length) Map.empty[String, String]
      else
        formals.zip(actuals).collect {
          case (formal, actual) if formal.isPhantom =>
            formal.getName(using defGetSet) -> printer.csDFValRef(actual, call.getOwner)
        }.toMap
    defPrinter.setPhantomActuals(phantomActuals)
    defPrinter
  end methodPrinterAt
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
    meta.docOpt.map(printer.csDocString).mkString("\n")
  def csAnnotations(annotations: List[annotation.HWAnnotation]): String
  final def csDFMember(member: DFMember): String =
    val cs = member match
      case dfVal: DFVal.CanBeExpr if dfVal.isAnonymous => csDFValExpr(dfVal)
      case dfVal: DFVal                                => csDFValNamed(dfVal)
      case net: DFNet                                  => csDFNet(net)
      case inst: DFDesignInst                          =>
        inst.getDesignBlock.instMode match
          case InstMode.Def => csMethodInst(inst)
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
    sn"""|${printer.csDocString(member.meta)}
         |${printer.csAnnotations(member.meta.annotations)}
         |$cs"""
  end csDFMember
  def designFileName(designName: String): String
  def globalFileName: String

  // ── HDL-method global-emission decision ─────────────────────────────────────
  // Which HDL-method blocks (ED methods / static functions) are emitted ONCE in the shared
  // globals area (a VHDL package / a Verilog defs header) instead of inlined in each using
  // design. This is a PLACEMENT decision computed purely from the IR, and it is
  // BACKEND-SPECIFIC (VHDL additionally globalizes a static function read by a port
  // declaration, since the entity is elaborated before the architecture), so it lives in the
  // printer as an overridable `def` rather than in the IR. The compiler pipeline feeds the
  // printer a flat DB, so these read the design members directly (no sub-DB routing).

  // HDL-method blocks mapped to the set of NON-method designs that use them. A method call is
  // owned by the design (or method) whose body makes the call (`designBlockOwnershipMap`); a
  // method-to-method call is resolved transitively, so the resulting users are always real
  // designs.
  private def hdlMethodDesignUsers: Map[DFDesignBlock, Set[DFDesignBlock]] =
    val ownership = getSet.designDB.designBlockOwnershipMap
    def realUsersOf(block: DFDesignBlock, seen: Set[DFDesignBlock]): Set[DFDesignBlock] =
      ownership.getOrElse(block, Set.empty).flatMap { owner =>
        if (!owner.isHDLMethod) Set(owner)
        else if (seen(owner)) Set.empty[DFDesignBlock]
        else realUsersOf(owner, seen + owner)
      }
    ownership.keysIterator.filter(_.isHDLMethod)
      .map(m => m -> realUsersOf(m, Set(m))).toMap

  // The body members of an HDL-method block: the members it owns.
  protected final def methodBodyMembers(m: DFDesignBlock): List[DFMember] =
    getSet.designDB.designMemberTable.getOrElse(m, Nil)

  // An HDL method is emittable in a shared package/header only if its body references no value
  // captured from a single design. Captures materialize as PHANTOM input ports (globals are
  // never captured — they are reachable everywhere and referenced directly), so a method with
  // any phantom input is inherently design-local and stays inlined there.
  protected final def methodIsGlobalEligible(m: DFDesignBlock): Boolean =
    !methodBodyMembers(m).exists {
      case dcl: DFVal.Dcl => dcl.isPortIn && dcl.isPhantom
      case _              => false
    }

  // HDL-method blocks referenced by a GLOBAL `Func` call (a static function called at global
  // scope, e.g. to compute a global constant). Such a method has no design user, but must still
  // be emitted once in the shared globals area alongside the global value it computes.
  private def globalCallMethods: Set[DFDesignBlock] =
    getSet.designDB.membersGlobals.view.collect {
      case DFVal.Func.Call(_, key) => key.getDesignBlock
    }.filter(_.isHDLMethod).toSet

  // HDL-method blocks emitted once in the shared globals area: used by more than one design, or
  // called from global scope; and package-eligible. Overridable per backend (VHDL adds static
  // functions read by a port declaration).
  def globalHDLMethods: Set[DFDesignBlock] =
    val byUsage = hdlMethodDesignUsers.iterator.collect {
      case (m, users) if users.sizeIs > 1 => m
    }
    (byUsage.toSet ++ globalCallMethods).filter(methodIsGlobalEligible)

  protected def hasGlobalContentCheck: Boolean =
    val designDB = getSet.designDB
    val anyNamedGlobal =
      if (designDB.isRoot)
        designDB.subDBs.view.values.exists(_.membersGlobals.exists(!_.isAnonymous))
      else designDB.membersGlobals.exists(!_.isAnonymous)
    anyNamedGlobal || csGlobalTypeDcls.nonEmpty ||
    globalHDLMethods.nonEmpty
  lazy val hasGlobalContent: Boolean = hasGlobalContentCheck
  // Global constants and global HDL methods in DEPENDENCY order. Both HDLs require a name to be
  // declared before it is used, and the dependency between the two runs BOTH ways: a constant's
  // value may CALL a method (a width computed by a static function), while a method's body may
  // READ a global constant. A fixed constants-then-methods split is therefore wrong in one
  // direction, so the order is a stable topological sort over the actual references, keeping
  // declaration order among independent declarations. A reference cycle (representable only in
  // VHDL, where a prototype and its body are split) falls back to declaration order.
  protected final def globalDeclsOrdered: List[GlobalDecl] =
    val constsWithPrinters = globalConstsWithPrinters
    val globalConsts: List[DFMember] = constsWithPrinters.map(_._2)
    val constDeclOf: Map[DFMember, GlobalDecl] =
      globalConsts.view.map(c => c -> GlobalDecl.Const(c)).toMap
    val methodBlocks: List[DFDesignBlock] = globalMethodPrinters.map(_._1)
    val methodSet = methodBlocks.toSet
    // the global declarations transitively referenced from `seeds`, excluding `self`. Resolved
    // under `gs`, the getSet of the declaration being scanned (on a hierarchical root each
    // global lives in its own sub-DB).
    def depsFrom(
        seeds: List[DFMember],
        self: Option[DFMember],
        gs: MemberGetSet
    ): List[GlobalDecl] =
      val found = mutable.LinkedHashSet.empty[GlobalDecl]
      val visited = mutable.Set.empty[DFMember]
      def visit(m: DFMember): Unit =
        if (visited.add(m))
          m match
            case DFVal.Func.Call(_, key) =>
              val block = key.getDesignBlock(using gs)
              if (methodSet.contains(block)) found += GlobalDecl.Method(block)
            case _ =>
          if (!self.contains(m)) constDeclOf.get(m).foreach(found += _)
          m.getRefs.foreach(r => visit(r.get(using gs)))
      seeds.foreach(visit)
      found.toList
    end depsFrom
    def depsOf(decl: GlobalDecl): List[GlobalDecl] = decl match
      case GlobalDecl.Const(c) =>
        depsFrom(List(c), Some(c), constPrinterOf.getOrElse(c, printer).getSet)
      case GlobalDecl.Method(b) =>
        depsFrom(methodBodyMembers(b), None, globalMethodPrinterOf(b).getSet)
          .filterNot(_ == decl)
    val ordered = mutable.ListBuffer.empty[GlobalDecl]
    val done = mutable.Set.empty[GlobalDecl]
    val onPath = mutable.Set.empty[GlobalDecl]
    def place(decl: GlobalDecl): Unit =
      if (!done.contains(decl) && onPath.add(decl))
        depsOf(decl).foreach(place)
        onPath -= decl
        done += decl
        ordered += decl
    (globalConsts.map(GlobalDecl.Const(_)) ++ methodBlocks.map(GlobalDecl.Method(_)))
      .foreach(place)
    ordered.toList
  end globalDeclsOrdered

  private lazy val globalMethodPrinterOf: Map[DFDesignBlock, TPrinter] =
    globalMethodPrinters.toMap
  private lazy val constPrinterOf: Map[DFMember, TPrinter] =
    globalConstsWithPrinters.view.map((p, c) => c -> p).toMap
  // one global declaration rendered as a DEFINITION (a constant declaration, or a method with
  // its body). VHDL renders the method half as a prototype in its package spec instead.
  protected final def csGlobalDecl(decl: GlobalDecl): String = decl match
    case GlobalDecl.Const(c)  => constPrinterOf.getOrElse(c, printer).csDFMembers(List(c))
    case GlobalDecl.Method(b) => globalMethodPrinterOf(b).csMethodDcl(b).stripTrailing
  protected final def csGlobalDecls: String =
    globalDeclsOrdered.map(csGlobalDecl).filter(_.nonEmpty).mkString("\n")

  def csGlobalFileContent: String =
    sn"""|$csGlobalTypeDcls
         |$csGlobalDecls"""
  // The global HDL methods (ED methods / static functions used across designs or from
  // global scope) rendered as method DEFINITIONS, in post-order (a method after the
  // methods it calls). Shared by the single-string DB view (`csDB`) and the backends'
  // globals file (where they are additionally wrapped — a VHDL package body, a Verilog
  // defs header). Empty for a design with no global methods.
  def csGlobalMethodDcls: String =
    // `csMethodDcl` ends with a trailing newline (needed when it stands alone as a file),
    // so strip it here before joining — otherwise it doubles up with the blank line that
    // separates the globals block from the rest.
    globalMethodPrinters.map((block, p) => p.csMethodDcl(block).stripTrailing).mkString("\n\n")
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
      case InstMode.Def => csMethodDcl(design)
      case _            => csDFDesignBlockDcl(design)
    // a foreign IP renders as a bare `import` of its external class, so its doc comment (carried on
    // the IP class) must not be emitted ahead of the import
    val docString = if (design.isExternalIPBlackbox) "" else csDocString(design.dclMeta)
    sn"""|$docString
         |$designDcl"""
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
    val printers =
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
    // methods with phantoms print their declaration locally in the host design's
    // body (see `printMethodDclInline`), and HDL methods are locally scoped — they
    // print inside their owning design (see `methodPrinters`); neither prints as a
    // file-level declaration
    printers.filterNot((block, _) => printMethodDclInline(block) || block.isHDLMethod)
  end designPrinters

  // The (HDL method design, printer-bound-to-its-getSet) pairs locally declared by `design`.
  // ED methods and static functions print inside their owning design's declaration; they are
  // discovered through the DFDesignInst members of `design` (including calls made inside process
  // blocks) AND, transitively, of the method bodies themselves — one called only from
  // another's body is declared in the host design just the same, and would otherwise never
  // be emitted at all.
  //
  // The order is post-order (a method follows the methods it calls), because an HDL
  // method must be declared before it is used. Each is bound to its FIRST call site's
  // printer, which resolves its phantom actuals in that call site's scope.
  final def methodPrinters(design: DFDesignBlock): List[(DFDesignBlock, TPrinter)] =
    // methods emitted once in the shared globals area (used by more than one design, or
    // from global scope) are excluded here — the using design references them by call,
    // and they are declared globally rather than inside each design (see
    // `globalMethodPrinters` and the backends' globals-file emission)
    val globals = globalHDLMethods
    val ordered = mutable.ListBuffer.empty[(DFDesignBlock, TPrinter)]
    val visited = mutable.Set.empty[DFDesignBlock]
    def visit(hostPrinter: TPrinter, host: DFDesignBlock): Unit =
      host.members(MemberView.Flattened)(using hostPrinter.getSet).foreach {
        case DFVal.Func.Call(call, designKey) =>
          val block = designKey.getDesignBlock(using hostPrinter.getSet)
          // `visited` is marked before recursing, so a (plugin-rejected) recursive method
          // cannot loop here
          if (block.isHDLMethod && !globals.contains(block) && visited.add(block))
            // every concrete printer is its own TPrinter (`given printer: TPrinter = this`),
            // so `hostPrinter.TPrinter` IS this printer's TPrinter — a fact the path-dependent
            // type cannot express
            val methodPrinter = hostPrinter.methodPrinterAt(call, designKey).asInstanceOf[TPrinter]
            visit(methodPrinter, block)
            ordered += block -> methodPrinter
        case _ =>
      }
    visit(printer, design)
    ordered.toList
  end methodPrinters

  // The (global HDL method design, printer-bound-to-its-getSet) pairs to emit ONCE in the
  // shared globals area (a VHDL package / a Verilog defs header). A global method is used
  // by more than one design (or from global scope) and captures nothing design-local (no
  // phantoms), so it needs no call-site phantom substitution. Ordered post-order (a method
  // follows the methods it calls) since an HDL method must be declared before it is used.
  final def globalMethodPrinters: List[(DFDesignBlock, TPrinter)] =
    val root = getSet.designDB.rootDB
    val globals = globalHDLMethods
    val ordered = mutable.ListBuffer.empty[(DFDesignBlock, TPrinter)]
    val visited = mutable.Set.empty[DFDesignBlock]
    def blockPrinterAndMembers(block: DFDesignBlock): (TPrinter, List[DFMember], MemberGetSet) =
      root.subDBs.get(block.ownerRef) match
        case Some(sub) => (withGetSet(sub.getSet), sub.members, sub.getSet)
        case None      => (printer, block.members(MemberView.Folded), getSet)
    def visit(block: DFDesignBlock): Unit =
      if (globals.contains(block) && visited.add(block))
        val (p, members, defGetSet) = blockPrinterAndMembers(block)
        // recurse into callees first (post-order)
        members.foreach {
          case DFVal.Func.Call(_, key) => visit(key.getDesignBlock(using defGetSet))
          case _                       =>
        }
        ordered += block -> p
    // deterministic outer order: the sub-DB (elaboration) order of the global methods
    val orderedGlobals =
      if (root.isRoot) root.subDBs.view.values.map(_.top).filter(globals.contains)
      else globals.view
    orderedGlobals.foreach(visit)
    ordered.toList
  end globalMethodPrinters

  final def csDB: String =
    // a foreign IP renders as an `import <clsName>` of its pre-existing external class; multiple
    // foreign IP design blocks may share the same class, so emit each distinct import only once.
    val seenForeignImports = mutable.Set.empty[String]
    val csFileList = designPrinters.collect {
      case (block, p)
          if printerOptions.designPrintFilter(block) &&
            // external IP blackboxes (vendor IP, foreign IP) are not rendered as modules by the
            // HDL backends — vendor IP is generated by the vendor tool, foreign IP ships its HDL
            // wrapper as a resource. The DFHDL printer (printVendorIPBlackbox) still renders them.
            (!block.isExternalIPBlackbox || printVendorIPBlackbox) &&
            block.foreignIPSource.forall(src => seenForeignImports.add(src.clsName)) =>
        formatCode(p.csFile(block))
    }
    val globals = formatCode(
      sn"""|$csGlobalTypeDcls
           |$csGlobalDecls"""
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
    if (wait.isEndless) "wait"
    else
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
    end if
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
    else annotations.view.map(_.codeString).mkString("\n")
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
    "case", "final", "for", "while", "until", "to", "by", "import", "this"
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
